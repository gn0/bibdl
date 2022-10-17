
# `bibdl`: download a paper's BibTeX entry using its journal link

## Installation and usage

To install `bibdl`, clone this repository and copy `bibdl.rkt` to your local directory of executables:

```
git clone https://codeberg.org/gnyeki/bibdl.git
[ -d ~/.local/bin ] || mkdir -p ~/.local/bin
cd bibdl && cp bibdl.rkt ~/.local/bin/bibdl && chmod +x ~/.local/bin/bibdl
```

If you are using bash, then run

```
(echo $PATH | tr ':' '\n' | grep -q ~/.local/bin) || echo 'export PATH=$HOME/.local/bin:$PATH' >> ~/.bashrc
source ~/.bashrc
```

Now you can use `bibdl` to download BibTeX citation data.
For example:

```
$ bibdl 'https://academic.oup.com/restud/article-abstract/60/3/531/1570385?redirectedFrom=fulltext'
Written to 'manski1993restud.bib'.
$ cat manski1993restud.bib
@article{manski1993restud,
    author = {Manski, Charles F.},
    title = "{Identification of Endogenous Social Effects: The Reflection Problem}",
    journal = {The Review of Economic Studies},
    volume = {60},
    number = {3},
    pages = {531-542},
    year = {1993},
    month = {07},
    abstract = "{This paper examines the reflection problem that arises when a researcher observing the distribution of behaviour in a population tries to infer whether the average behaviour in some group influences the behaviour of the individuals that comprise the group. It is found that inference is not possible unless the researcher has prior information specifying the compisition of reference groups. If this information is available, the prospects for inference depend critically on the population relationship between the variables defining reference groups and those directly affecting outcomes. Inference is difficult to implossible if these variables are functionally dependent or are statistically independent. The prospects are better if the variables defining reference groups and those directly affecting outcomes are moderately related in the population.}",
    issn = {0034-6527},
    doi = {10.2307/2298123},
    url = {https://doi.org/10.2307/2298123},
    eprint = {https://academic.oup.com/restud/article-pdf/60/3/531/4468725/60-3-531.pdf},
}
$
```

## To do

- [ ] Add fetcher for ECTA.
- [ ] Add fetcher for REStat.
- [ ] Add fetcher for JPubE.
- [ ] Add proper CLI argument parser.
- [ ] Add support for plumbing vs. porcelain.


