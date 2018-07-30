
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/gederajeg/collogetr.svg?branch=master)](https://travis-ci.org/gederajeg/collogetr) [![Coverage Status](https://img.shields.io/codecov/c/github/gederajeg/collogetr/master.svg)](https://codecov.io/github/gederajeg/collogetr?branch=master)

collogetr
=========

Overview
--------

`collogetr` has one function (viz. `colloc_leipzig()`) to retrieve window-span collocates for a set of word forms (viz. the *node word*) from the (Indonesian) [Leipzig Corpora](http://wortschatz.uni-leipzig.de/en/download). There are two functions to process the output of `colloc_leipzig()` into tabular formats as input for **association measure** between the collocates and the node, as in Stefanowitsch and Gries' ([2003](#ref-stefanowitsch_collostructions_2003)) [collostructional/collocation analysis](http://www.linguistics.ucsb.edu/faculty/stgries/teaching/groningen/index.html) (see also, Gries, [2015](#ref-gries_more_2015); Stefanowitsch, [2013](#ref-hoffmann_collostructional_2013); Stefanowitsch & Gries, [2009](#ref-stefanowitsch_corpora_2009)). These functions are `assoc_prepare()` and `assoc_prepare_dca()`. The former generates input for [*Simple Collexeme/Collocational Analysis*](http://www.linguistics.ucsb.edu/faculty/stgries/research/2003_AS-STG_Collostructions_IJCL.pdf) computed using `collex_fye()`, meanwhile the latter uses the output of `assoc_prepare()` to generate input for [*Distinctive Collexeme/Collocates Analysis*](http://www.linguistics.ucsb.edu/faculty/stgries/research/2004_STG-AS_ExtendingCollostructions_IJCL.pdf) (Gries & Stefanowitsch, [2004](#ref-gries_extending_2004); Hilpert, [2006](#ref-hilpert_distinctive_2006)) computed using `collex_fye_dca()`. `collogetr` is built on top of the core packages in the [tidyverse](https://www.tidyverse.org).

Installation
------------

Install from GitHub with [devtools](https://github.com/hadley/devtools):

``` r
library(devtools)
install_github("gederajeg/collogetr")
```

Usages
------

### Load `collogetr`

``` r
library(collogetr)
```

### Citation `collogetr`

To cite the package in publication, type as follows:

``` r
citation("collogetr")
#> 
#> To cite `collogetr` in publication, please use:
#> 
#>   Rajeg, G. P. W. (2018). collogetr: Collocates retriever and
#>   collocational association measure for the Indonesian Leipzig
#>   Corpora. R package development version 1.0.0. url:
#>   https://github.com/gederajeg/collogetr
#> 
#> Please also cite the following foundational works on the Collexeme
#> Analysis and Distinctive Collexeme Analysis:
#> 
#>   Stefanowitsch, A., & Gries, S. T. (2003). Collostructions:
#>   Investigating the interaction of words and constructions.
#>   International Journal of Corpus Linguistics, 8(2), 209–243.
#> 
#>   Gries, S. T., & Stefanowitsch, A. (2004). Extending
#>   collostructional analysis: A corpus-based perspective on
#>   ‘alternations’. International Journal of Corpus Linguistics,
#>   9(1), 97–129.
#> 
#> To see these entries in BibTeX format, use 'print(<citation>,
#> bibtex=TRUE)', 'toBibtex(.)', or set
#> 'options(citation.bibtex.max=999)'.
```

### Package data

The package has three data sets for demonstration. The important one is the `demo_corpus_leipzig` whose documentation can be accessed via `?demo_corpus_leipzig`. Another data is a list of Indonesian stopwords (i.e. `stopwords`) that can be filtered out when performing collocational measure. The last one is `leipzig_corpus_path` containing character vector of full path to my Leipzig Corpus files in my computer.

#### Accepted inputs

`colloc_leipzig()` accepts two types of input data:

1.  A named-list object with character-vector elements of each Leipzig Corpus Files, represented by `demo_corpus_leipzig` and the format of which is shown below:

``` r
lapply(demo_corpus_leipzig[1:2], sample, 2)
#> $ind_mixed_2012_1M
#> [1] "180504 ” SUNGGUH sebuah kehormatan besar bagiku untuk mengenal dan belajar ilmu agama dari Kiai Sholeh Darat salah seorang wali Allah terkemuka ini."                                                                                 
#> [2] "169725 Karena Yesus dalam kemanusiaanNya itu disebut “Anak Abraham” dan “Anak Daud “ ( Matius 1:1), maka haruslah dalam jasad daging kemanusiaanNya itu mengalir “gen” dari Abraham dan Daud bapa-bapa leluhurNya secara manusia itu."
#> 
#> $ind_news_2008_300K
#> [1] "81638 \"Sekitar 80 persen produk kakao Indonesia dihasilkan oleh petani perorangan."                
#> [2] "20744 Setelah beberapa jam pertempuran itu terhenti tetapi suara tembakan sporadis masih terdengar."
```

1.  Full-paths to the Leipzig Corpus plain texts, as in the `leipzig_corpus_path`.

``` r
leipzig_corpus_path[1:2]
#> [1] "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora/ind_mixed_2012_1M-sentences.txt" 
#> [2] "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora/ind_news_2008_300K-sentences.txt"
```

### Demo

#### Retrieving the collocates

Here is how to retrieve the collocates for the Indonesian verb *mengatakan* 'to say sth.'. The function will print out progress messages of the stages onto the console. It generates warning(s) when a search pattern or node word is not found in a corpus file or in all loaded corpus files.

``` r
out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
                       pattern = "mengatakan",
                       window = "r",
                       span = 1L,
                       save_interim = FALSE)
```

In the example above, the collocates are restricted to those occurring *one* word (i.e. `span = 1L`) to the *right* (`window = "r"`) of *mengatakan* 'to say'. The `"r"` character in `window` stands for *right*-side collocates (`"l"` for *left*-side collocates and `"b"` for *both* right- and left-side collocates). The `span` argument requires integer (i.e., a whole number) to indicate the range of words covered in the specified window. The `pattern` argument requires one or more exact word forms; if more than one, put into a character vector (e.g., `c("mengatakan", "menjanjikan")`). The `pattern` argument also accepts strings marked with *word boundary* character (i.e. `"\\b"`), thus `c("\\bmengatakan\\b", "\\bmenjanjikan\\b")`.

The `save_interim` is `FALSE` means that no output is saved into the computer, but in the console (i.e., in the `out` object). If `save_interim = TRUE`, the function will save the outputs into the files in the computer. `colloc_leipzig()` has specified the default file names for the outputs via these arguments: (i) `freqlist_output_file`, (ii) `colloc_output_file`, (iii) `corpussize_output_file`, and (iv) `search_pattern_output_file`. It is recommended that the output filenames are stored as a character vector. See **Examples** "(2)" in the documentation of `colloc_leipzig()` for a call when `save_interim = TRUE`.

#### Exploring the output of `colloc_leipzig()`.

The output of `colloc_leipzig()` is a list of 4 elements:

1.  `colloc_df`; a table/tibble of raw collocates data with columns for:
    1.  corpus names
    2.  sentence id in which the collocates and the node word(s) are found
    3.  the collocates (column `w`)
    4.  the span information (e.g., `"r1"` for one-word, right-side collocates)
    5.  the node word
    6.  the text/sentence match in which the collocates and the node are found
2.  `freqlist_df`; a table/tibble of word-frequency list in the loaded corpus
3.  `corpussize_df`; a table/tibble of total word-tokens in the loaded corpus
4.  `pattern`; a character vector of the search pattern/node

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

The `freqlist_df` and `corpussize_df` are important for performing the collocational strength measure for the search pattern with the collocates.

#### Preparing input data for *Simple Collexeme/Collocational Analysis* (SCA).

First we need to call `assoc_prepare()` for generating the data for SCA with `collex_fye()`. The demo illustrates it with on-console output of `colloc_leipzig()`. Cf. the **Examples** "2.2" in the documentation for `assoc_prepare()` for handling saved outputs (`?assoc_prepare()`).

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

Inspect the output of `assoc_prepare()`

``` r
head(assoc_tb)
#> # A tibble: 6 x 3
#>   w          node       data            
#>   <chr>      <chr>      <list>          
#> 1 pemerintah mengatakan <tibble [1 × 9]>
#> 2 israel     mengatakan <tibble [1 × 9]>
#> 3 kasus      mengatakan <tibble [1 × 9]>
#> 4 akibat     mengatakan <tibble [1 × 9]>
#> 5 alokasi    mengatakan <tibble [1 × 9]>
#> 6 angklung   mengatakan <tibble [1 × 9]>
```

The `assoc_prepare()` and `collex_fye()` functions are designed following the tidy principle so that the association/collocation measure is performed in a row-wise fashion, benefiting from the combination of [*nested* column](http://r4ds.had.co.nz/many-models.html#list-columns-1) (cf., Wickham & Grolemund, [2017](#ref-wickham_r_2017), p. 409) for the input-data (using `tidyr::nest()`) and `purrr`'s `map_*` function. `assoc_prepare()` includes calculating the expected co-occurrence frequencies between the collocates/collexemes and the node word/construction.

The column `data` in `assoc_tb` above consists of nested tibble/table as a list. Each contains required data for performing association measure for each of the collocates in column `w` (Gries, [2013](#ref-gries_50-something_2013), [2015](#ref-gries_more_2015); Stefanowitsch & Gries, [2003](#ref-stefanowitsch_collostructions_2003), [2009](#ref-stefanowitsch_corpora_2009)). This nested column can be inspected as follows (for the first row, namely for the word *pihaknya* 'the party').

``` r
# get the tibble in the `data` column for the first row
assoc_tb$data[[1]]
#> # A tibble: 1 x 9
#>   a_exp     a n_w_in_corp corpus_size n_pattern     b     c     d assoc   
#>   <dbl> <int>       <int>       <int>     <int> <int> <int> <int> <chr>   
#> 1 0.354     4          96       41179       152    92   148 40935 attract…
```

Column `a` indicates the co-occurrence frequency between the node word and the collocates column `w`, meanwhile `a_exp` indicates the *expected co-occurrence frequency* between them. The `n_w_in_corp` represents the total token/occurrence frequency of a given collocate. The `n_pattern` stores the total token/occurrence frequency of the node word in the corpus. Column `b`, `c`, and `d` are required for the association measure that is essentially based on 2-by-2 crosstabulation table. The `assoc` column indicates whether the value in `a` is higher than that in `a_exp`, thus indicating *attraction* or *positive association* between the node word and the collocate. The reverse is *repulsion* or *negative association* when the value in `a` is less/lower than that in `a_exp`.

#### *Simple Collexeme/Collocates Analysis (SCA)*

As in the *Collostructional Analysis* (Stefanowitsch & Gries, [2003](#ref-stefanowitsch_collostructions_2003)), `collex_fye()` uses one-tailed *Fisher-Yates Exact* test whose *p*-<sub>FisherExact</sub>value is log-transformed to the base of 10 to indicate the collostruction strength between the collocates and the node word (Gries, Hampe, & Schönefeld, [2005](#ref-gries_converging_2005)). `collex_fye()` simultaneously performs two uni-directional measures of *Delta P* (Gries, [2013](#ref-gries_50-something_2013), [2015](#ref-gries_more_2015), p. 524). One of these shows the extent to which the presence of the node-word cues the presence of the collocates/collexemes; the other one determines the extent to which the collocates/collexemes cues the presence of the node-word.

Here is the code to perform the SCA

``` r
# perform FYE test for Collexeme Analysis
am_fye <- collex_fye(df = assoc_tb, collstr_digit = 3)
```

Now we can retrieve the top-10 most strongly attracted collocates to *mengatakan* 'to say sth.'. The association strength is shown in the `collstr` column, which stands for *collostruction strength*. The higher, the stronger the association.

``` r
# get the top-10 most strongly attracted collocates
dplyr::top_n(am_fye, 10, collstr)
#> # A tibble: 16 x 9
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
#> 11 lily  meng…     1 0.004 attr… 3.69e-3    2.43            0.007
#> 12 peme… meng…     1 0.004 attr… 3.69e-3    2.43            0.007
#> 13 pemo… meng…     1 0.004 attr… 3.69e-3    2.43            0.007
#> 14 ptn   meng…     1 0.004 attr… 3.69e-3    2.43            0.007
#> 15 ukm   meng…     1 0.004 attr… 3.69e-3    2.43            0.007
#> 16 wna   meng…     1 0.004 attr… 3.69e-3    2.43            0.007
#> # ... with 1 more variable: dP_cxn_cue_collex <dbl>
```

Column `a` contains the co-occurrence frequency of the collocates (`w`) with the `node` as its R1 collocates in the demo corpus. `p_fye` shows the one-tailed *p*<sub>FisherExact</sub>-value. Updated README file is prepared for retrieving data to perform *Distinctive Collexeme Analysis*.

### Session info

``` r
devtools::session_info()
#> Session info -------------------------------------------------------------
#>  setting  value                       
#>  version  R version 3.5.1 (2018-07-02)
#>  system   x86_64, darwin15.6.0        
#>  ui       X11                         
#>  language (EN)                        
#>  collate  en_US.UTF-8                 
#>  tz       Australia/Melbourne         
#>  date     2018-07-30
#> Packages -----------------------------------------------------------------
#>  package    * version date       source                              
#>  assertthat   0.2.0   2017-04-11 CRAN (R 3.4.0)                      
#>  backports    1.1.2   2017-12-13 CRAN (R 3.5.0)                      
#>  base       * 3.5.1   2018-07-05 local                               
#>  bindr        0.1.1   2018-03-13 cran (@0.1.1)                       
#>  bindrcpp   * 0.2.2   2018-03-29 CRAN (R 3.5.0)                      
#>  cli          1.0.0   2017-11-05 CRAN (R 3.4.2)                      
#>  collogetr  * 1.0.1   2018-07-30 Github (gederajeg/collogetr@1e4f0e7)
#>  compiler     3.5.1   2018-07-05 local                               
#>  crayon       1.3.4   2017-09-16 CRAN (R 3.4.1)                      
#>  datasets   * 3.5.1   2018-07-05 local                               
#>  devtools     1.13.6  2018-06-27 CRAN (R 3.5.0)                      
#>  digest       0.6.15  2018-01-28 CRAN (R 3.5.0)                      
#>  dplyr        0.7.6   2018-06-29 CRAN (R 3.5.1)                      
#>  evaluate     0.11    2018-07-17 CRAN (R 3.5.0)                      
#>  fansi        0.2.3   2018-05-06 CRAN (R 3.5.0)                      
#>  glue         1.3.0   2018-07-17 CRAN (R 3.5.0)                      
#>  graphics   * 3.5.1   2018-07-05 local                               
#>  grDevices  * 3.5.1   2018-07-05 local                               
#>  hms          0.4.2   2018-03-10 cran (@0.4.2)                       
#>  htmltools    0.3.6   2017-04-28 CRAN (R 3.5.0)                      
#>  knitr        1.20    2018-02-20 CRAN (R 3.5.0)                      
#>  magrittr     1.5     2014-11-22 CRAN (R 3.4.0)                      
#>  memoise      1.1.0   2017-04-21 CRAN (R 3.4.0)                      
#>  methods    * 3.5.1   2018-07-05 local                               
#>  pillar       1.3.0   2018-07-14 CRAN (R 3.5.0)                      
#>  pkgconfig    2.0.1   2017-03-21 CRAN (R 3.4.0)                      
#>  purrr        0.2.5   2018-05-29 CRAN (R 3.5.0)                      
#>  R6           2.2.2   2017-06-17 CRAN (R 3.4.0)                      
#>  Rcpp         0.12.18 2018-07-23 CRAN (R 3.5.1)                      
#>  readr        1.1.1   2017-05-16 CRAN (R 3.5.0)                      
#>  rlang        0.2.1   2018-05-30 CRAN (R 3.5.0)                      
#>  rmarkdown    1.10    2018-06-11 CRAN (R 3.5.0)                      
#>  rprojroot    1.3-2   2018-01-03 CRAN (R 3.4.3)                      
#>  stats      * 3.5.1   2018-07-05 local                               
#>  stringi      1.2.4   2018-07-20 CRAN (R 3.5.0)                      
#>  stringr      1.3.1   2018-05-10 cran (@1.3.1)                       
#>  tibble       1.4.2   2018-01-22 CRAN (R 3.5.0)                      
#>  tidyr        0.8.1   2018-05-18 CRAN (R 3.5.0)                      
#>  tidyselect   0.2.4   2018-02-26 CRAN (R 3.5.0)                      
#>  tools        3.5.1   2018-07-05 local                               
#>  utf8         1.1.4   2018-05-24 CRAN (R 3.5.0)                      
#>  utils      * 3.5.1   2018-07-05 local                               
#>  withr        2.1.2   2018-03-15 cran (@2.1.2)                       
#>  yaml         2.1.19  2018-05-01 CRAN (R 3.5.0)
```

### References

Gries, S. T. (2013). 50-something years of work on collocations: What is or should be next …. *International Journal of Corpus Linguistics*, *18*(1), 137–166. doi:[10.1075/ijcl.18.1.09gri](https://doi.org/10.1075/ijcl.18.1.09gri)

Gries, S. T. (2015). More (old and new) misunderstandings of collostructional analysis: On Schmid and Küchenhoff (2013). *Cognitive Linguistics*, *26*(3), 505–536. doi:[10.1515/cog-2014-0092](https://doi.org/10.1515/cog-2014-0092)

Gries, S. T., & Stefanowitsch, A. (2004). Extending collostructional analysis: A corpus-based perspective on ’alternations’. *International Journal of Corpus Linguistics*, *9*(1), 97–129.

Gries, S. T., Hampe, B., & Schönefeld, D. (2005). Converging evidence: Bringing together experimental and corpus data on the association of verbs and constructions. *Cognitive Linguistics*, *16*(4), 635–676.

Hilpert, M. (2006). Distinctive collexeme analysis and diachrony. *Corpus Linguistics and Linguistic Theory*, *2*(2), 243–256.

Stefanowitsch, A. (2013). Collostructional analysis. In T. Hoffmann & G. Trousdale (Eds.), *The Oxford handbook of Construction Grammar* (pp. 290–306). Oxford: Oxford University Press. doi:[10.1093/oxfordhb/9780195396683.013.0016](https://doi.org/10.1093/oxfordhb/9780195396683.013.0016)

Stefanowitsch, A., & Gries, S. T. (2003). Collostructions: Investigating the interaction of words and constructions. *International Journal of Corpus Linguistics*, *8*(2), 209–243.

Stefanowitsch, A., & Gries, S. T. (2009). Corpora and grammar. In A. Lüdeling & M. Kytö (Eds.), *Corpus linguistics: An international handbook* (Vol. 2, pp. 933–951). Berlin: Mouton de Gruyter.

Wickham, H., & Grolemund, G. (2017). *R for Data Science*. Canada: O’Reilly. Retrieved from <http://r4ds.had.co.nz/>
