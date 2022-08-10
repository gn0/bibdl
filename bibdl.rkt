#!/usr/bin/env racket
#lang racket

(require net/url)

;;
;; Functions to access journal websites.
;;

(define (clean-content content)
  (string-append
   (string-replace (string-trim content)
                   "\r\n"
                   "\n")
   "\n"))

(define (fetch-url url)
  (clean-content
   (port->string
    (get-pure-port
     (string->url url)))))

;; AEA journals.  Example URL:
;;
;; https://www.aeaweb.org/articles?id=10.1257/pol.2.4.26
;;

(define (aeaweb-url? url)
  (regexp-match? #rx"^https?://www[.]aeaweb[.]org/articles[?]id=" url))

(define (aeaweb-url->doi url)
  (let ([match (regexp-match #rx"/articles[?]id=([^&]+)" url)])
    (cadr match)))

(define (fetch-aeaweb url)
  (let ([doi (aeaweb-url->doi url)])
    (fetch-url (string-append
                "https://www.aeaweb.org/articles/citation-export"
                "?args%5Bformat%5D=bib"
                "&args%5Bdoi%5D=" (string-replace doi "/" "%2F")
                "&args%5Btype%5D=txt"))))

;;
;; Functions to manipulate bibliography entries.
;;

(define (first-letter word)
  (substring word 0 1))

(define (get-initials str)
  (let ([words (regexp-split #rx" +" str)])
    (string-downcase
     (string-join (map first-letter words) ""))))

(define (bib->year content)
  (let ([match (regexp-match #rx"(?i:year) *=[{\" ]*([0-9]+)[}\" ]*,"
                             content)])
    (if (not match)
        (error 'bib->year-parse-error)
        (cadr match))))

(define (raw-author->author raw-author)
  (if (string-contains? raw-author ",")
      (let ([raw-names (regexp-split #rx", *" raw-author)])
        (map string-trim raw-names))
      (let ([match (regexp-match #rx"(.*?) ([^ ]+)$" raw-author)])
        (if (not match)
            (error 'raw-author->author-parse-error)
            (let ([raw-names (list (caddr match) (cadr match))])
              (map string-trim raw-names))))))

(define (bib->authors content)
  (let* ([brace-match (regexp-match #rx"(?i:author) *= *{(.+?)},"
                                    content)]
         [quote-match (regexp-match #rx"(?i:author) *= *\"(.+?)\","
                                    content)]
         [match (if brace-match brace-match quote-match)])
    (if (not match)
        (error 'bib->authors-parse-error)
        (let* ([raw-author-string (cadr match)]
               [raw-authors (string-split raw-author-string " and ")]
               [authors (map raw-author->author raw-authors)])
          authors))))

(define (authors->abbrev authors)
  (cond
    [(empty? authors) #f]
    [(= (length authors) 1) (string-downcase (caar authors))]
    [(> (length authors) 1) (get-initials
                             (string-join (map car authors)))]))

(define (bib->journal content)
  (let ([match (regexp-match #rx"(?i:journal) *= *([^,]+),"
                             content)])
    (if (not match)
        #f
        (let ([raw-journal (cadr match)])
          (string-trim
           (regexp-replaces raw-journal '([#rx"[{}\"]" ""]
                                          [#rx" +" " "])))))))

(define (journal-qje? journal)
  (regexp-match-exact? #rx"(?i:(the )?quarterly journal of economics)"
                       journal))

(define (journal-aer? journal)
  (regexp-match-exact? #rx"(?i:(the )?american economic review)"
                       journal))

(define (journal-aeri? journal)
  (regexp-match-exact?
   #rx"(?i:(the )american economic review: insights)"
   journal))

(define (journal-ecta? journal)
  (regexp-match? #rx"^(?i:econometrica)($|:)" journal))

(define (journal-restud? journal)
  (regexp-match-exact? #rx"(?i:(the )?review of economic studies)"
                       journal))

(define (journal-restat? journal)
  (regexp-match-exact?
   #rx"(?i:(the )?review of economics and statistics)"
   journal))

(define (journal-aejapp? journal)
  (regexp-match-exact?
   #rx"(?i:american economic journal: applied( economics)?)"
   journal))

(define (journal-aejpol? journal)
  (regexp-match-exact?
   #rx"(?i:american economic journal: economic policy)"
   journal))

(define (journal-aejmic? journal)
  (regexp-match-exact?
   #rx"(?i:american economic journal: microeconomics)"
   journal))

(define (journal-aejmac? journal)
  (regexp-match-exact?
   #rx"(?i:american economic journal: macroeconomics)"
   journal))

(define (journal->abbrev journal)
  (cond
    [(journal-qje? journal) "qje"]
    [(journal-aer? journal) "aer"]
    [(journal-aeri? journal) "aeri"]
    [(journal-ecta? journal) "ecta"]
    [(journal-restud? journal) "restud"]
    [(journal-restat? journal) "restat"]
    [(journal-aejapp? journal) "aejapp"]
    [(journal-aejpol? journal) "aejpol"]
    [(journal-aejmic? journal) "aejmic"]
    [(journal-aejmac? journal) "aejmac"]
    [else (get-initials journal)]))

(define (bib->bib-id content)
  (let ([match (regexp-match #rx"{([^,]+)," content)])
    (if (not match)
        (error 'bib->bib-id-parse-error)
        (cadr match))))

(define (build-bib-id content)
  (let* ([year (bib->year content)]
         [authors (bib->authors content)]
         [author-abbrev (authors->abbrev authors)]
         [id-stub (string-append author-abbrev year)]
         [journal (bib->journal content)])
    (if (not journal)
        id-stub
        (string-append id-stub (journal->abbrev journal)))))

(define (replace-bib-id content new-bib-id)
  (regexp-replace
   #rx"{[^,]*,"
   content
   (string-append "{" new-bib-id ",")))

(define (update-bib-id content)
  (replace-bib-id content
                  (build-bib-id content)))

(define (write-bib filename content)
  (let ([handle (open-output-file filename)])
    (write-string content handle)
    (close-output-port handle)))

(define (url-to-fetcher url)
  (cond
    [(aeaweb-url? url) fetch-aeaweb]
    [else (error 'unrecognized-url)]))

(define (same-content? a b)
  (eq? (string-trim a "\n")
       (string-trim b "\n")))

(define (main args)
  (if (not (= (length args) 1))
      (displayln "Usage: bibdl <url>")
      (let* ([url (car args)]
             [bib-content (update-bib-id
                           ((url-to-fetcher url) url))]
             [bib-id (bib->bib-id bib-content)]
             [output-filename (string-append bib-id ".bib")])
        (if (file-exists? output-filename)
            (let ([file-content (file->string output-filename)])
              (if (same-content? file-content bib-content)
                  ;; FIXME Even when file-content and bib-content are
                  ;;       the same, we don't end up in this branch.
                  ;;       Why?
                  (printf "Already downloaded this.  See '~a'.\n"
                          output-filename)
                  (begin
                    (printf
                     "error: Output filename '~a' already exists.\n\n"
                     output-filename)
                    (printf "Contents of '~a':\n" output-filename)
                    (displayln "----")
                    (displayln (string-trim file-content))
                    (displayln "----\n")
                    (displayln "The downloaded BibTeX entry:")
                    (displayln "----")
                    (displayln (string-trim bib-content))
                    (displayln "----"))))
            (begin
              (write-bib output-filename bib-content)
              (printf "Written to '~a'.\n" output-filename))))))

(main (vector->list (current-command-line-arguments)))