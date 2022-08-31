#!/usr/bin/env racket
#lang racket

(require net/url)

;;
;; Functions to access journal websites.
;;

(define (clean-content content)
  (string-append
   (string-trim
    (string-replace content "\r\n" "\n"))
   "\n"))

(define (download-url url)
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
    (download-url (string-append
                   "https://www.aeaweb.org/articles/citation-export"
                   "?args%5Bformat%5D=bib"
                   "&args%5Bdoi%5D=" (string-replace doi "/" "%2F")
                   "&args%5Btype%5D=txt"))))

;; Cambridge University Press journals.  Example URL:
;;
;; https://www.cambridge.org/core/journals/american-political-science-review/article/abs/using-a-probabilistic-model-to-assist-merging-of-largescale-administrative-records/DB2955F64A1F4E262C5B9B26C6D7552E
;;

(define (cambridgeup-url? url)
  (regexp-match?
   #rx"^https?://www[.]cambridge[.]org/core/journals/[^/]+/article/"
   url))

(define (cambridgeup-url->id url)
  (let ([match (regexp-match #px"/([0-9A-F]{32})" url)])
    (cadr match)))

(define (fetch-cambridgeup url)
  (let ([id (cambridgeup-url->id url)])
    (download-url
     (string-append
      "https://www.cambridge.org/core/services/aop-easybib/export"
      "?exportType=bibtex&productIds=" id
      "&citationStyle=bibtex"))))

;; JSTOR journals.  Example URL:
;;
;; https://www.jstor.org/stable/3598804#metadata_info_tab_contents
;;

(define (jstor-url? url)
  (regexp-match?
   #rx"^https?://www[.]jstor[.]org/stable/[1-9][0-9]*"
   url))

(define (jstor-url->id url)
  (let ([match (regexp-match #px"/([1-9][0-9]*)" url)])
    (cadr match)))

(define (fetch-jstor url)
  (let ([id (jstor-url->id url)])
    (download-url
     (string-append "https://www.jstor.org/citation/text/" id))))

;; Oxford University Press journals.  Example URL:
;;
;; https://academic.oup.com/qje/article-abstract/114/3/739/1848099?redirectedFrom=fulltext
;;

(define (oxfordup-url? url)
  (regexp-match?
   #rx"^https?://academic[.]oup[.]com/[^/]+/[^/]+/[0-9]+/"
   url))

(define (oxfordup-url->id url)
  (let ([match (regexp-match #rx"/([0-9]+)($|[?])" url)])
    (cadr match)))

(define (fetch-oxfordup url)
  (let ([id (oxfordup-url->id url)])
    (download-url
     (string-append "https://academic.oup.com/Citation/Download"
                    "?resourceId=" id
                    "&resourceType=3"
                    "&citationFormat=2"))))

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
  (let ([match (regexp-match #rx"(?i:year) *=[{\" ]*([0-9]+)[}\" ]*"
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
    [(= (length authors) 1) (string-downcase
                             (regexp-replace* #rx"[- ]+"
                                              (caar authors)
                                              ""))]
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

(define (journal-jae? journal)
  (regexp-match-exact? #rx"(?i:journal of african economies)" journal))

(define (journal->abbrev journal)
  (cond
    [(journal-qje?    journal) "qje"]
    [(journal-aer?    journal) "aer"]
    [(journal-aeri?   journal) "aeri"]
    [(journal-ecta?   journal) "ecta"]
    [(journal-restud? journal) "restud"]
    [(journal-restat? journal) "restat"]
    [(journal-aejapp? journal) "aejapp"]
    [(journal-aejpol? journal) "aejpol"]
    [(journal-aejmic? journal) "aejmic"]
    [(journal-aejmac? journal) "aejmac"]
    [(journal-jae?    journal) "jae"]
    [else                      (get-initials journal)]))

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

(define (url->fetcher url)
  (cond
    [(aeaweb-url?      url) fetch-aeaweb]
    [(cambridgeup-url? url) fetch-cambridgeup]
    [(jstor-url?       url) fetch-jstor]
    [(oxfordup-url?    url) fetch-oxfordup]
    [else                   (error 'unrecognized-url)]))

(define (same-content? a b)
  (string=? (string-trim a "\n")
            (string-trim b "\n")))

(define (main args)
  (if (or (not (= (length args) 1))
          (member (car args) '("--help" "-h")))
      (displayln "Usage: bibdl <url>")
      (let* ([url (car args)]
             [bib-content (update-bib-id
                           ((url->fetcher url) url))]
             [bib-id (bib->bib-id bib-content)]
             [output-filename (string-append bib-id ".bib")])
        (if (file-exists? output-filename)
            (let ([file-content (file->string output-filename)])
              (if (same-content? file-content bib-content)
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
