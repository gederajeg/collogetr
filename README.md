
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis-CI Build
Status](https://travis-ci.org/gederajeg/collogetr.svg?branch=master)](https://travis-ci.org/gederajeg/collogetr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/gederajeg/collogetr?branch=master&svg=true)](https://ci.appveyor.com/project/gederajeg/collogetr)
[![Coverage
Status](https://img.shields.io/codecov/c/github/gederajeg/collogetr/master.svg)](https://codecov.io/github/gederajeg/collogetr?branch=master)
[![DOI](https://img.shields.io/badge/doi-10.26180/5b7b9c5e32779-blue.svg?style=flat&labelColor=gainsboro&logoWidth=40&logo=data%3Aimage%2Fpng%3Bbase64%2CiVBORw0KGgoAAAANSUhEUgAAAFAAAAAZCAYAAACmRqkJAAAKi0lEQVR4Ae3ZaVBUV97H8evuE0EfH32MmkcfoyAuGjXKgkvMaFRAFuiloemWvRuEXlgEBREXBYJiXAQUFeKocUniQiKogAJhQWwWENDEjLNYvjFLzUzNkplEZb5kTme6nCRjKlOpSZlb9SmL2%2Ffcuv3re87%2FnKP0TYfOcslqPMbt63xBKuh09MTxgi7HKT1Sj1TvKp%2BMkZB6%2FXT8c4AjUYPyVdfb7Qs6HTIJ8EHe7Ul%2B152CphDabRQ0uMr7%2FRQgh%2B8qU6%2FBiPDVGv0jq0uGE94b0ZZ3j%2B25MTetoMsh%2FWD91OBqT9%2Fsehd5EqGV17nKMzTqOHvaRMMLEp7qACfinq%2FW1BBx5ZxB13x5X3Jr1v%2Fz9pUcaHU63PiicjrhvXfNRbY1Th49Q6Y1vu6zyqSjzX3aVIgf4OkKToxhgxpd5OMzV0bYE4CRN1Chu34pnTfwnV03FiTlfzDRXBHo6dfgIq8sX6ByV6vjthGc0UdrrPPVGFQBxlSjzJQWENVUZkebceiLpyM8IZSx7O7Zl4JivUNMZX5h8Rt4%2B2L0llKfgu6JKa%2BXvpB5bZ48%2Ba3F6lil2pDkE2rODzCsU0VUnNFHNZQqdS3lx3Utl%2FMILQcfYt5TEeC1GSprgAq0XlgYGLQyxJTlr0uK0DVX7E5s2ZtOgHvLw5fLK9xVmcqguEj%2F2LXbwsvPBkZZKl4j5NcIKinaUsLbejFWZ7m8Do2cmwnb4cFqArRwx3TEYzi%2Bz7DTD0uhxnj8cAEWWUZK%2BTcdhh4pmTWUsW01Y1uCUmNY7Rtqzo5svJSS0poVXtg6yVj7sn9qunek3j8xPVXXeMFoaDkev6lDF7ene7Y5r2taNAXmEBXaP69zevaOjuUeeZ0zhzJuPsM5CdYvOhZVqBMhBqIVDt8zwGdQjR4of9AA%2BXJjUFpww7GodnHAQca4srDAWCXjW3pETal%2BbfumuOLKqSm17vIQtWr1Uu3JYy6JbXuXFbRN1R8pm5byxtG5CcdOz9EUVc7I5IeQEWQ7wWVwzwrsRn%2BbAFeiCxNsKv5Y9P03BFgjAlT90AGOQy2T47fObl00ocFZHl%2B2UGXw0RjzNUWHTPFthckHWh18al8KsGuaFigVVzlKuY%2BG9z37qvuoGlelpsJVldrgrFjbOE%2BeWe8uW18W84qCqc4s7tmCIgzI75hs%2FaJKNFu7rF%2BIIIhr%2BmIQ%2Btn8LQkDMQOeWAYnDHgsQI3NNU7W9j4h5t72o%2FEyvLEQ%2F%2Bu7ymzbOxbCAeOxAgtghz6YgOVYiufEOUlqu0M37ho%2BYn%2FnpJT8bsejVSt90uqdFdlGmV7hF7cuWXetNCShLX%2BI3nKhN%2ByvCs%2Bs6GQpWB33fzKNQR%2BqWr022yvc94q7spBCY%2Bbzkou6ZfJNPf89ZN%2FdidYHnIsKfIzjCMIc7MAwSJiMPFxGMcKQixGwx07R%2FiEe4CNsxFCbAJvwifj8LkIgYRHa8Lm47jNY8AokmMS5NryPh%2FijOB%2BOX4h7foEuyPHlisMtylJpzu1YspkQ36YbLqnx8F1X4abaqmYs9DGmLlrk4CE9XlHlKZskxfpt%2FUJLzyhV23dG%2BITF72fqo9njEaokwIu8lSbG1N4wx273CrP%2B%2BjniQVZhGrzQjlEioFIRcjDM6MIdjBVtHogvl4W9qIX8sTfwU5SgU%2FzdhdGYLcJ9BzvRID6vgx2SxN8PUI9KnIEWH4n7FuIo%2FoRfYV5vMMV4wHRFs%2BvG%2FKl05ZrDVdP11T7eulK3oNQcz%2FAXcj3DpMePjO44KetDL2lDh%2FmV1S3nNoeWnJb7RSXmMJl%2BI0GmH13rKs8lvEdQwfoWKmCxdmGbAEdgAW5jFiQhBb8WXSYTPSjGCBHaMPR5LMANkOCM%2B%2FgD3MS5Z8W1ElzwW3HNJCSI9tcw2ub%2BO8T5LPTBQBy1nusNcB7ztximI1sIsSSzXb04v3vyusJmx63nMufHXlV6LvpEShDd9x%2FHFYWXVPuSX7%2FD7zmpcjuWRupbyvaHnj8Z7BNsUFCArm70iTRcd5bFEN7oxwJs%2FpoA%2FwfBaLJ2Z2EFbmEsNKL7fYYPUI9DIqj%2Fsgkw0CasW%2BL6RbBDFI7gTZSKzz6Gk02AJ23G3QF4xybYU8INce6s5CJNlTyXhYwKv%2FRWMiEeimquzIhrPpGzuSNCsbvLec2%2Brpmh2e0yu%2FxOp96wv6p8X0xeIZW5Bo2%2F6ucdvb%2FdMWVDm8lX11pRpD16OJ6VyZsrQ8yK%2BVFJ9h4UhwEHDj5JgGE23UkSfoZujMMzSESNCPBT9KAFjqi2rcIYZRPgYmzDQ9xDLSz4%2FGsCPIE%2BNkWrTJy%2FhRrRthpVyJJExbnmG2I%2B6x%2BT%2FHxYyQkzQfJGlufpWy6bYlvPUEgu%2BHlHJA5boo7rE3blnBR7r6mv%2BvCBMYEag%2Faqsyr1%2BIk5a%2Fd2z9zGBDpZ31qulCWk9443Hfg5BuJJAgxAG0ZBEmS4DZ7RKIliMVi0d8UvRUCeuPoNAf4Z%2FmgV13pAwiwR3iffFKBQJM5noB%2F6Y5h45v7Wwf0cDtD1DlMIeiugWmZOy5Cv3RgjX7%2FF4GdMXasOjgurmqdafqpojltml9IjvOJ8NMu9lNL5gQmXdMu0BTefz8loMyoJvivs3VMZvhpjqaig%2FZ8gwJGYIsIKRh%2FY4wh%2Bg%2FGQoxYbREgZ%2BB3uww1V3xKgN%2BrwCNtF4Pvx8NveQCEYX%2BAukhCIYuHZLy%2FyDjHbJQfo7PTK1dEBWqPBX2vS%2B2hNW1XquDURypiwXStCjVWuyrSKQC%2FdoUaHtOT2HENoyal4b40x7rK7ylip9NIV3Jy0P6fD24fl3Ra6uoe3PNqOH2Pw3x%2FC8K8CHIU%2BIpQ7OI8yNOJ9TMJO%2FAU9Nn6PjRiGmm%2FpwgsRLQpKjwjuU%2Fz1CQK0R4G4T4%2FwCHWYKlmcA6xr4SA2EzobXeUa9vh21LgpdKxK8hqd5RsaXWS7S9YvlhU2O7ya3ekXrm%2B9lK3KzFH6a4y5V92Ve5hkM4d02EShMestZekE2IxZX7MWdkAgBtmsi9U2lXEwliAOK%2BGLTowThWIZkrEVSSKYgegPOUxwtFmdaBGLsRgg2qeKtosQDh2GYzbisUIEaPvcQ8T5VGzCKowBk2I3mTVALe4wd4tumKcoaZirSKte4RtVrvXwLrw%2BJXV%2F18Ts3BtLEmOaS0yRtRdMfpGJhTKNMbDJWR5V7eEbUNDtcIQAd1PJMwnuJl6E9KQHY7AAHkzQoBkj8B%2B%2FpTWQ4Maezne1P3x1esLBuqmB%2BbccNhJMGetbM%2BGZIi1V%2FoRyOXB77sKVWuPmrd4RBvYQm9ihVue%2F7xDPGljB50MoJmO%2By36gCGsQovCyCGwOarD9R7PLLXZOJjKZvse%2FDQQSvffG7F1rWrZPiLKUX2DPr1hbfHAKb0kDBSeTed5MQj94Pn1xBMvA%2B2IDYTAkcXzXANPRjHq04ACeFeH9aAIcBC3LOq%2FY5pPDeYtO4yRTmzUhbx9LozCEea8ybaHoxDNmVtPltxSVzxhCm3Asg4Tvs683Aa5wwkD8qP9XbgQqUbb6Tp09U5Os3rWiV4jZv2OuvxPdvht70RfST8fjATZd7P33OYzxZ%2FdF7FwcgqPU0yMR2vMYDulpDfBvw%2BGCdBePpq8AAAAASUVORK5CYII%3D)](http://dx.doi.org/10.26180/5b7b9c5e32779)

# collogetr

## Overview

collogetr performs (i) collocates retrieval (currently from the
sentence-based (Indonesian) [Leipzig
Corpora](http://wortschatz.uni-leipzig.de/en/download)) and (ii)
computation of collocation association-measures. The function
`colloc_leipzig()` is used to retrieve window-span collocates for a set
of word forms (viz. the *nodeword(s)* or *keyword(s)*).

Two other functions (namely, `assoc_prepare()` and
`assoc_prepare_dca()`) serve to process the output of `colloc_leipzig()`
into tabular/data frame formats, which then become the input data for
computing the **association measure** between the collocates and the
node (as in Stefanowitsch and Gries’
([2003](#ref-stefanowitsch_collostructions_2003))
[collostructional/collocation
analysis](http://www.linguistics.ucsb.edu/faculty/stgries/teaching/groningen/index.html))
(Stefanowitsch, [2013](#ref-hoffmann_collostructional_2013);
Stefanowitsch & Gries, [2009](#ref-stefanowitsch_corpora_2009); see
also, Gries, [2015](#ref-gries_more_2015)). The function
`assoc_prepare()` generates input data for computing [*Simple
Collexeme/Collocational
Analysis*](http://www.linguistics.ucsb.edu/faculty/stgries/research/2003_AS-STG_Collostructions_IJCL.pdf)
(SCA), meanwhile `assoc_prepare_dca()` uses the output of
`assoc_prepare()` to generate input data for computing [*Distinctive
Collexeme/Collocates
Analysis*](http://www.linguistics.ucsb.edu/faculty/stgries/research/2004_STG-AS_ExtendingCollostructions_IJCL.pdf)
(DCA) (Gries & Stefanowitsch, [2004](#ref-gries_extending_2004);
Hilpert, [2006](#ref-hilpert_distinctive_2006)). Based on the output of
`assoc_prepare()`, SCA can then be computed using `collex_fye()`, which
is based on Fisher-Yates Exact test (or `collex_llr()`, which is based
on Log-Likelihood Ratio); DCA is computed using `collex_fye_dca()`.

collogetr is built on top of the core packages in the
[tidyverse](https://www.tidyverse.org).

## Installation

Install collogetr from GitHub with
[devtools](https://github.com/hadley/devtools):

``` r
library(devtools)
install_github("gederajeg/collogetr")
```

## Usages

### Load collogetr

``` r
library(collogetr)
```

### Citation for collogetr

To cite collogetr in publication, type as follows:

``` r
citation("collogetr")
#> 
#> To cite `collogetr` in publication, please use:
#> 
#>   Rajeg, G. P. W. (2020). collogetr: Collocates retriever and
#>   collocational association measure. R package development version
#>   1.1.4. url: https://github.com/gederajeg/collogetr. doi:
#>   https://doi.org/10.26180/5b7b9c5e32779
#> 
#> Please also cite the following foundational works on the Collexeme
#> Analysis and Distinctive Collexeme Analysis:
#> 
#>   Stefanowitsch, A., & Gries, S. T. (2003). Collostructions:
#>   Investigating the interaction of words and constructions.
#>   International Journal of Corpus Linguistics, 8(2), 209–243.
#> 
#>   Gries, S. T., & Stefanowitsch, A. (2004). Extending collostructional
#>   analysis: A corpus-based perspective on ‘alternations’. International
#>   Journal of Corpus Linguistics, 9(1), 97–129.
#> 
#> To see these entries in BibTeX format, use 'print(<citation>,
#> bibtex=TRUE)', 'toBibtex(.)', or set
#> 'options(citation.bibtex.max=999)'.
```

### Package data

The package has three data sets for demonstration. The important one is
the `demo_corpus_leipzig` whose documentation can be accessed via
`?demo_corpus_leipzig`. Another data is a list of Indonesian stopwords
(i.e. `stopwords`) that can be filtered out when performing
collocational measure. The last one is `leipzig_corpus_path` containing
character vector of full path to my Leipzig Corpus files in my computer.

#### Accepted inputs

`colloc_leipzig()` accepts two types of corpus-input data:

1.  A named-list object with character-vector elements of each Leipzig
    Corpus Files, represented by `demo_corpus_leipzig` and the format of
    which is shown below:

<!-- end list -->

``` r
lapply(demo_corpus_leipzig[1:2], sample, 2)
#> $ind_mixed_2012_1M
#> [1] "201972 Menghisap rakyat sampai kering."                                                                               
#> [2] "422324 Tantangan yang disertai dengan penghormatan adalah suatu aspek yang penting dari pendekatan yang berhati-hati."
#> 
#> $ind_news_2008_300K
#> [1] "87424 Prangko pertama menampilkan gambar Soekarno sedang memberikan souvenir berupa keris kepada Fidel Castro, sedangkan pada prangko kedua ditampilkan gambar Soekarno sedang berbincang dengan Che Guevara."
#> [2] "226495 \"Bahkan, Imam Samudra sempat bertanya dimana saya mengisi khotbah Idulfitri 1429 H. Pak Ustadz, akan memberi khotbah Idul Fitri di mana?"
```

2.  Full-paths to the Leipzig Corpus plain texts, as in the
    `leipzig_corpus_path`.

<!-- end list -->

``` r
leipzig_corpus_path[1:2]
#> [1] "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora/ind_mixed_2012_1M-sentences.txt" 
#> [2] "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora/ind_news_2008_300K-sentences.txt"
```

In terms of the input strings for the `pattern` argument,
`colloc_leipzig()` accepts three scenarios:

1.  Plain string representing a whole word form, such as `"memberikan"`
    ‘to give’

2.  Regex of a whole word, such as `"^memberikan$"` ‘to give’

3.  Regex of a whole word with word boundary character (`\\b`), such as
    `"\\bmemberikan\\b"`.

All of these three forms will be used to match the exact word form of
the search pattern after the corpus file is tokenised into individual
words. That is, input patterns following scenario 1 or 3 will be turned
into their exact search pattern represented in scenario 2 (i.e., with
the beginning- and end-of-line anchors, hence `"^...$"`). So user can
directly use the input pattern in scenario 2 for the `pattern` argument.
If there are more than one word to be searched, put them into a
character vector (e.g., `c("^memberi$", "^membawa$")`).

### Demo

#### Retrieving the collocates

The codes below show how one may retrieve the collocates for the
Indonesian verb *mengatakan* ‘to say sth.’. The function
`colloc_leipzig()` will print out progress messages of the stages onto
the console. It generates warning(s) when a search pattern or node word
is not found in a corpus file or in all loaded corpus files.

``` r
out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
                       pattern = "mengatakan",
                       window = "r",
                       span = 1L,
                       save_interim = FALSE)
```

In the example above, the collocates are restricted to those occurring
*one* word (i.e. `span = 1L`) to the *right* (`window = "r"`) of
*mengatakan* ‘to say’. The `"r"` character in `window` stands for
*right*-side collocates (`"l"` for *left*-side collocates and `"b"` for
*both* right- and left-side collocates). The `span` argument requires
integer (i.e., a whole number) to indicate the range of words covered in
the specified window. The `pattern` argument requires one or more exact
word forms; if more than one, put into a character vector (e.g.,
`c("mengatakan", "menjanjikan")`).

The `save_interim` is `FALSE` means that no output is saved into the
computer, but in the console (i.e., in the `out` object). If
`save_interim = TRUE`, the function will save the outputs into the files
in the computer. `colloc_leipzig()` has specified the default file names
for the outputs via these arguments: (i) `freqlist_output_file`, (ii)
`colloc_output_file`, (iii) `corpussize_output_file`, and (iv)
`search_pattern_output_file`. It is recommended that the output
filenames are stored as a character vector. See **Examples** “(2)” in
the documentation of `colloc_leipzig()` for a call when `save_interim =
TRUE`.

#### Exploring the output of `colloc_leipzig()`.

The output of `colloc_leipzig()` is a list of 4 elements:

1.  `colloc_df`; a table/tibble of raw collocates data with columns for:
    1.  corpus names
    2.  sentence id in which the collocates and the node word(s) are
        found
    3.  the collocates (column `w`)
    4.  the span information (e.g., `"r1"` for one-word, right-side
        collocates)
    5.  the node word
    6.  the text/sentence match in which the collocates and the node are
        found
2.  `freqlist_df`; a table/tibble of word-frequency list in the loaded
    corpus
3.  `corpussize_df`; a table/tibble of total word-tokens in the loaded
    corpus
4.  `pattern`; a character vector of the search pattern/node

<!-- end list -->

``` r
str(out)
#> List of 4
#>  $ colloc_df    :Classes 'tbl_df', 'tbl' and 'data.frame':   151 obs. of  6 variables:
#>   ..$ corpus_names: chr [1:151] "ind_mixed_2012_1M" "ind_mixed_2012_1M" "ind_mixed_2012_1M" "ind_news_2008_300K" ...
#>   ..$ sent_id     : int [1:151] 185 191 215 1 93 96 122 130 136 158 ...
#>   ..$ w           : chr [1:151] "kalau" "ia" "bahwa" "rupiah" ...
#>   ..$ span        : chr [1:151] "r1" "r1" "r1" "r1" ...
#>   ..$ node        : chr [1:151] "mengatakan" "mengatakan" "mengatakan" "mengatakan" ...
#>   ..$ sent_match  : chr [1:151] "705166 Beberapa kawan mengatakan kalau voting dilakukan secara tertutup satu orang satu suara dan tidak ada kes"| __truncated__ "870266 Pak haji mengatakan, ia sebenarnya menginginkan seorang menantu yang bisa mengajarkan caranya menggunaka"| __truncated__ "256689 Catatan: sebelum bagian ini Edwin Louis Cole mengatakan bahwa Allah memberikan firman kepada Martin Luth"| __truncated__ "270199 Ia mengatakan, rupiah makin terpuruk sulit dipertahankan, karena faktor negatif internal sangat kuat men"| __truncated__ ...
#>  $ freqlist_df  :Classes 'tbl_df', 'tbl' and 'data.frame':   30093 obs. of  3 variables:
#>   ..$ corpus_names: chr [1:30093] "ind_mixed_2012_1M" "ind_mixed_2012_1M" "ind_mixed_2012_1M" "ind_mixed_2012_1M" ...
#>   ..$ w           : chr [1:30093] "yang" "dan" "di" "dengan" ...
#>   ..$ n           : int [1:30093] 128 93 59 53 50 45 37 32 31 28 ...
#>  $ corpussize_df:Classes 'tbl_df', 'tbl' and 'data.frame':   15 obs. of  2 variables:
#>   ..$ corpus_names: chr [1:15] "ind_mixed_2012_1M" "ind_news_2008_300K" "ind_news_2009_300K" "ind_news_2010_300K" ...
#>   ..$ size        : int [1:15] 3676 4663 4740 4904 4690 4881 4018 3854 3831 3827 ...
#>  $ pattern      : chr "mengatakan"
```

The `freqlist_df` and `corpussize_df` are important for performing the
collocational strength measure for the search pattern with the
collocates.

#### Preparing input data for *Simple Collexeme/Collocational Analysis* (SCA).

First we need to call `assoc_prepare()` for generating the data SCA. The
demo illustrates it with in-console output of `colloc_leipzig()`. See
the **Examples** “2.2” in the documentation for `assoc_prepare()` for
handling saved outputs (`?assoc_prepare()`).

``` r
assoc_tb <- assoc_prepare(colloc_out = out, 
                          window_span = "r1",
                          per_corpus = FALSE, # combine all data across corpus
                          stopword_list = collogetr::stopwords,
                          float_digits = 3L)
#> Your colloc_leipzig output is stored as list!
#> You chose to combine the collocational and frequency list data from ALL CORPORA!
#> Tallying frequency list of all words in ALL CORPORA!
#> You chose to remove stopwords!
```

Inspect the output of `assoc_prepare()`:

``` r
head(assoc_tb)
#> # A tibble: 6 x 3
#> # Groups:   node, w [6]
#>   node       w          data            
#>   <chr>      <chr>      <list>          
#> 1 mengatakan pemerintah <tibble [1 × 9]>
#> 2 mengatakan israel     <tibble [1 × 9]>
#> 3 mengatakan kasus      <tibble [1 × 9]>
#> 4 mengatakan akibat     <tibble [1 × 9]>
#> 5 mengatakan alokasi    <tibble [1 × 9]>
#> 6 mengatakan angklung   <tibble [1 × 9]>
```

The `assoc_prepare()` and `collex_fye()` functions are designed
following the tidy principle so that the association/collocation measure
is performed in a row-wise fashion, benefiting from the combination of
[*nested* column](http://r4ds.had.co.nz/many-models.html#list-columns-1)
(cf., Wickham & Grolemund, [2017](#ref-wickham_r_2017), p. 409) for the
input-data (using `tidyr::nest()`) and `purrr`’s `map_*` function.
`assoc_prepare()` includes calculating the expected co-occurrence
frequencies between the collocates/collexemes and the node
word/construction.

The column `data` in `assoc_tb` above consists of nested tibble/table as
a list. Each contains required data for performing association measure
for each of the collocates in column `w` (Gries,
[2013](#ref-gries_50-something_2013), [2015](#ref-gries_more_2015);
Stefanowitsch & Gries, [2003](#ref-stefanowitsch_collostructions_2003),
[2009](#ref-stefanowitsch_corpora_2009)). This nested column can be
inspected as follows (for the first row, namely for the word *pihaknya*
‘the party’).

``` r
# get the tibble in the `data` column for the first row
assoc_tb$data[[1]]
#> # A tibble: 1 x 9
#>       a n_w_in_corp corpus_size n_pattern     b     c     d a_exp assoc     
#>   <int>       <int>       <int>     <int> <int> <int> <int> <dbl> <chr>     
#> 1     4          96       41179       152    92   148 40935 0.354 attraction
```

Column `a` indicates the co-occurrence frequency between the node word
and the collocates column `w`, meanwhile `a_exp` indicates the *expected
co-occurrence frequency* between them. The `n_w_in_corp` represents the
total token/occurrence frequency of a given collocate. The `n_pattern`
stores the total token/occurrence frequency of the node word in the
corpus. Column `b`, `c`, and `d` are required for the association
measure that is essentially based on 2-by-2 crosstabulation table. The
`assoc` column indicates whether the value in `a` is higher than that in
`a_exp`, thus indicating *attraction* or *positive association* between
the node word and the collocate. The reverse is *repulsion* or *negative
association* when the value in `a` is less/lower than that in `a_exp`.

#### *Simple Collexeme/Collocates Analysis (SCA)*

As in the *Collostructional Analysis* (Stefanowitsch & Gries,
[2003](#ref-stefanowitsch_collostructions_2003)), `collex_fye()` uses
one-tailed *Fisher-Yates Exact* test whose
*p*-<sub>FisherExact</sub>value is log-transformed to the base of 10 to
indicate the collostruction strength between the collocates and the node
word (Gries, Hampe, & Schönefeld, [2005](#ref-gries_converging_2005)).
`collex_fye()` simultaneously performs two uni-directional measures of
*Delta P* (Gries, [2013](#ref-gries_50-something_2013),
[2015](#ref-gries_more_2015), p. 524). One of these shows the extent to
which the presence of the node-word cues the presence of the
collocates/collexemes; the other one determines the extent to which the
collocates/collexemes cues the presence of the node-word.

Here is the codes to perform the SCA using `collex_fye()`:

``` r
# perform FYE test for Collexeme Analysis
am_fye <- collex_fye(df = assoc_tb, collstr_digit = 3)
```

Now we can retrieve the top-10 most strongly attracted collocates to
*mengatakan* ‘to say sth.’. The association strength is shown in the
`collstr` column, which stands for *collostruction strength*. The
higher, the stronger the association.

``` r
# get the top-10 most strongly attracted collocates
dplyr::top_n(am_fye, 10, collstr)
#> # A tibble: 84 x 9
#> # Groups:   node, w [84]
#>    w     node      a a_exp assoc   p_fye collstr dP_collex_cue_c…
#>    <chr> <chr> <int> <dbl> <chr>   <dbl>   <dbl>            <dbl>
#>  1 peme… meng…     4 0.354 attr… 4.55e-4    3.34            0.024
#>  2 isra… meng…     2 0.078 attr… 2.71e-3    2.57            0.013
#>  3 angk… meng…     1 0.004 attr… 3.69e-3    2.43            0.007
#>  4 ayla  meng…     1 0.004 attr… 3.69e-3    2.43            0.007
#>  5 defi… meng…     1 0.004 attr… 3.69e-3    2.43            0.007
#>  6 hofos meng…     1 0.004 attr… 3.69e-3    2.43            0.007
#>  7 kawa… meng…     1 0.004 attr… 3.69e-3    2.43            0.007
#>  8 kebe… meng…     1 0.004 attr… 3.69e-3    2.43            0.007
#>  9 kete… meng…     1 0.004 attr… 3.69e-3    2.43            0.007
#> 10 konj… meng…     1 0.004 attr… 3.69e-3    2.43            0.007
#> # … with 74 more rows, and 1 more variable: dP_cxn_cue_collex <dbl>
```

Column `a` contains the co-occurrence frequency of the collocates (`w`)
with the `node` as its R1 collocates in the demo corpus. `p_fye` shows
the one-tailed *p*<sub>FisherExact</sub>-value.

#### *Distinctive Collexeme/Collocate Analysis* (DCA)

The idea of distinctive collexemes/collocates is to contrast *two*
functionally/semantically similar constructions or words in terms of the
collocates that are (significantly) more frequent for one of the two
contrasted constructions/words (Hilpert,
[2006](#ref-hilpert_distinctive_2006); see Gries & Stefanowitsch,
[2004](#ref-gries_extending_2004)). `colloc_leipzig()` can be used to
retrieve collocates of *two* functionally/semantically similar words by
specifying the `pattern` argument with two character vectors of words.

The following example use one of the Leipzig corpus files (not included
in the package but can be downloaded from the Leipzig Corpora webpage
for free), namely the `"ind_mixed_2012_1M-sentences"`. The aim is to
contrast collocational preferences of two deadjectival transitive verbs
based on the root *kuat* ‘strong’ framed within two causative
morphological schemas: one with *per-*+ADJ and the other with
ADJ+*-kan*. Theoretically, the *per-* schema indicates that the direct
object of the verb is caused to have *more* of the characteristic
indicated by the adjectival root, meanwhile the *-kan* schema indicates
that the direct object is caused to have the characteristic indicated by
the root (that is not previously had). The focus here is on the R1
collocates of the verbs (i.e. one word immediately to the right of the
verbs in the sentences).

``` r
my_leipzig_path <- collogetr::leipzig_corpus_path[1]
dca_coll <- collogetr::colloc_leipzig(leipzig_path = my_leipzig_path, 
                                     pattern = c("memperkuat", "menguatkan"),
                                     window = "r",
                                     span = 1,
                                     save_interim = FALSE)
```

Then, we prepare the output into the format required for performing DCA
with `collex_fye_dca()`.

``` r
assoc_tb <- assoc_prepare(colloc_out = dca_coll,
                          window_span = "r1",
                          per_corpus = FALSE,
                          stopword_list = collogetr::stopwords,
                          float_digits = 3L)
#> Your colloc_leipzig output is stored as list!
#> You chose to combine the collocational and frequency list data from ALL CORPORA!
#> Tallying frequency list of all words in ALL CORPORA!
#> You chose to remove stopwords!

# prepare the dca input table
dca_tb <- assoc_prepare_dca(assoc_tb)
```

Compute DCA for the two verbs and view the results snippet.

``` r
dca_res <- collex_fye_dca(dca_tb)
head(dca_res, 10)
#> # A tibble: 10 x 6
#> # Groups:   w [10]
#>    w          memperkuat menguatkan   p_fye collstr dist_for  
#>    <chr>           <int>      <int>   <dbl>   <dbl> <chr>     
#>  1 diagnosis          15          0 0.00908   2.04  memperkuat
#>  2 posisi             15          0 0.00908   2.04  memperkuat
#>  3 daya               14          0 0.0125    1.90  memperkuat
#>  4 sistem             13          0 0.0171    1.77  memperkuat
#>  5 pertahanan         10          0 0.0439    1.36  memperkuat
#>  6 basis               8          0 0.0823    1.08  memperkuat
#>  7 ketahanan           7          0 0.113     0.948 memperkuat
#>  8 pasukan             7          0 0.113     0.948 memperkuat
#>  9 rasa                7          0 0.113     0.948 memperkuat
#> 10 tim                 7          0 0.113     0.948 memperkuat
```

The package also includes a function called `dca_top_collex()` to
retrieve the top-n distinctive collocates for one of the two contrasted
words. The `dist_for` argument can be specified by either the character
vector of the name of the contrasted words, or the character IDs of the
constructions/words (e.g., `..., dist_for = "a", ...` or `..., dist_for
= "A", ...` for construction/word appearing in the second column from
the output of `collex_fye_dca()`; `..., dist_for = "b", ...` or `...,
dist_for = "B", ...` for construction/word appearing in the third
column).

``` r
# retrieve distinctive collocates for Construction A (i.e., memperkuat)
dist_for_a <- dca_top_collex(dca_res, dist_for = "memperkuat", top_n = 10)
head(dist_for_a)
#> # A tibble: 6 x 6
#> # Groups:   w [6]
#>   w          memperkuat menguatkan   p_fye collstr dist_for  
#>   <chr>           <int>      <int>   <dbl>   <dbl> <chr>     
#> 1 diagnosis          15          0 0.00908    2.04 memperkuat
#> 2 posisi             15          0 0.00908    2.04 memperkuat
#> 3 daya               14          0 0.0125     1.90 memperkuat
#> 4 sistem             13          0 0.0171     1.77 memperkuat
#> 5 pertahanan         10          0 0.0439     1.36 memperkuat
#> 6 basis               8          0 0.0823     1.08 memperkuat
```

The codes below retrieve the distinctive collocates for *menguatkan* ‘to
strengthen’ or Construction B.

``` r
# retrieve distinctive collocates for Construction B (i.e., menguatkan)
dist_for_b <- dca_top_collex(dca_res, dist_for = "menguatkan", top_n = 10)
head(dist_for_b)
#> # A tibble: 6 x 6
#> # Groups:   w [6]
#>   w           memperkuat menguatkan       p_fye collstr dist_for  
#>   <chr>            <int>      <int>       <dbl>   <dbl> <chr>     
#> 1 hati                 0         12 0.000000109    6.96 menguatkan
#> 2 satu                 2          8 0.000636       3.20 menguatkan
#> 3 iman                 7         10 0.00487        2.31 menguatkan
#> 4 kebenaran            0          4 0.00501        2.3  menguatkan
#> 5 orang                0          4 0.00501        2.3  menguatkan
#> 6 kepercayaan          0          3 0.0189         1.72 menguatkan
```

### Session info

``` r
devtools::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value                       
#>  version  R version 3.6.0 (2019-04-26)
#>  os       macOS  10.15.3              
#>  system   x86_64, darwin15.6.0        
#>  ui       X11                         
#>  language (EN)                        
#>  collate  en_US.UTF-8                 
#>  ctype    en_US.UTF-8                 
#>  tz       Asia/Makassar               
#>  date     2020-03-27                  
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package     * version date       lib source        
#>  assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.6.0)
#>  backports     1.1.5   2019-10-02 [1] CRAN (R 3.6.0)
#>  callr         3.2.0   2019-03-15 [1] CRAN (R 3.6.0)
#>  cli           1.1.0   2019-03-19 [1] CRAN (R 3.6.0)
#>  collogetr   * 1.1.4   2020-03-26 [1] local         
#>  crayon        1.3.4   2017-09-16 [1] CRAN (R 3.6.0)
#>  desc          1.2.0   2018-05-01 [1] CRAN (R 3.6.0)
#>  devtools      2.2.1   2019-09-24 [1] CRAN (R 3.6.0)
#>  digest        0.6.23  2019-11-23 [1] CRAN (R 3.6.0)
#>  dplyr         0.8.3   2019-07-04 [1] CRAN (R 3.6.0)
#>  ellipsis      0.3.0   2019-09-20 [1] CRAN (R 3.6.0)
#>  evaluate      0.14    2019-05-28 [1] CRAN (R 3.6.0)
#>  fansi         0.4.0   2018-10-05 [1] CRAN (R 3.6.0)
#>  fs            1.3.1   2019-05-06 [1] CRAN (R 3.6.0)
#>  glue          1.3.1   2019-03-12 [1] CRAN (R 3.6.0)
#>  hms           0.5.2   2019-10-30 [1] CRAN (R 3.6.0)
#>  htmltools     0.3.6   2017-04-28 [1] CRAN (R 3.6.0)
#>  knitr         1.28    2020-02-06 [1] CRAN (R 3.6.0)
#>  lifecycle     0.2.0   2020-03-06 [1] CRAN (R 3.6.0)
#>  magrittr      1.5     2014-11-22 [1] CRAN (R 3.6.0)
#>  memoise       1.1.0   2017-04-21 [1] CRAN (R 3.6.0)
#>  pillar        1.4.2   2019-06-29 [1] CRAN (R 3.6.0)
#>  pkgbuild      1.0.3   2019-03-20 [1] CRAN (R 3.6.0)
#>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 3.6.0)
#>  pkgload       1.0.2   2018-10-29 [1] CRAN (R 3.6.0)
#>  prettyunits   1.0.2   2015-07-13 [1] CRAN (R 3.6.0)
#>  processx      3.3.1   2019-05-08 [1] CRAN (R 3.6.0)
#>  ps            1.3.0   2018-12-21 [1] CRAN (R 3.6.0)
#>  purrr         0.3.3   2019-10-18 [1] CRAN (R 3.6.0)
#>  R6            2.4.1   2019-11-12 [1] CRAN (R 3.6.0)
#>  Rcpp          1.0.3   2019-11-08 [1] CRAN (R 3.6.0)
#>  readr         1.3.1   2018-12-21 [1] CRAN (R 3.6.0)
#>  remotes       2.1.0   2019-06-24 [1] CRAN (R 3.6.0)
#>  rlang         0.4.3   2020-01-24 [1] CRAN (R 3.6.0)
#>  rmarkdown     2.1     2020-01-20 [1] CRAN (R 3.6.0)
#>  rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.6.0)
#>  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.6.0)
#>  stringi       1.4.5   2020-01-11 [1] CRAN (R 3.6.0)
#>  stringr       1.4.0   2019-02-10 [1] CRAN (R 3.6.0)
#>  testthat      2.3.1   2019-12-01 [1] CRAN (R 3.6.0)
#>  tibble        2.1.3   2019-06-06 [1] CRAN (R 3.6.0)
#>  tidyr         1.0.2   2020-01-24 [1] CRAN (R 3.6.0)
#>  tidyselect    0.2.5   2018-10-11 [1] CRAN (R 3.6.0)
#>  usethis       1.5.1   2019-07-04 [1] CRAN (R 3.6.0)
#>  utf8          1.1.4   2018-05-24 [1] CRAN (R 3.6.0)
#>  vctrs         0.2.2   2020-01-24 [1] CRAN (R 3.6.0)
#>  withr         2.1.2   2018-03-15 [1] CRAN (R 3.6.0)
#>  xfun          0.12    2020-01-13 [1] CRAN (R 3.6.0)
#>  yaml          2.2.0   2018-07-25 [1] CRAN (R 3.6.0)
#> 
#> [1] /Users/Primahadi/Rlibs
#> [2] /Library/Frameworks/R.framework/Versions/3.6/Resources/library
```

### References

<div id="refs" class="references">

<div id="ref-gries_50-something_2013">

Gries, S. T. (2013). 50-something years of work on collocations: What is
or should be next …. *International Journal of Corpus Linguistics*,
*18*(1), 137–166. <https://doi.org/10.1075/ijcl.18.1.09gri>

</div>

<div id="ref-gries_more_2015">

Gries, S. T. (2015). More (old and new) misunderstandings of
collostructional analysis: On Schmid and Küchenhoff (2013). *Cognitive
Linguistics*, *26*(3), 505–536. <https://doi.org/10.1515/cog-2014-0092>

</div>

<div id="ref-gries_converging_2005">

Gries, S. T., Hampe, B., & Schönefeld, D. (2005). Converging evidence:
Bringing together experimental and corpus data on the association of
verbs and constructions. *Cognitive Linguistics*, *16*(4), 635–676.

</div>

<div id="ref-gries_extending_2004">

Gries, S. T., & Stefanowitsch, A. (2004). Extending collostructional
analysis: A corpus-based perspective on ’alternations’. *International
Journal of Corpus Linguistics*, *9*(1), 97–129.

</div>

<div id="ref-hilpert_distinctive_2006">

Hilpert, M. (2006). Distinctive collexeme analysis and diachrony.
*Corpus Linguistics and Linguistic Theory*, *2*(2), 243–256.

</div>

<div id="ref-hoffmann_collostructional_2013">

Stefanowitsch, A. (2013). Collostructional analysis. In T. Hoffmann & G.
Trousdale (Eds.), *The Oxford handbook of Construction Grammar* (pp.
290–306). <https://doi.org/10.1093/oxfordhb/9780195396683.013.0016>

</div>

<div id="ref-stefanowitsch_collostructions_2003">

Stefanowitsch, A., & Gries, S. T. (2003). Collostructions: Investigating
the interaction of words and constructions. *International Journal of
Corpus Linguistics*, *8*(2), 209–243.

</div>

<div id="ref-stefanowitsch_corpora_2009">

Stefanowitsch, A., & Gries, S. T. (2009). Corpora and grammar. In A.
Lüdeling & M. Kytö (Eds.), *Corpus linguistics: An international
handbook* (Vol. 2, pp. 933–951). Berlin: Mouton de Gruyter.

</div>

<div id="ref-wickham_r_2017">

Wickham, H., & Grolemund, G. (2017). *R for Data Science*. Retrieved
from <http://r4ds.had.co.nz/>

</div>

</div>
