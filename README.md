
<!-- README.md is generated from README.Rmd. Please edit that file -->
**Authors:** [Gede Primahadi Wijaya Rajeg](https://figshare.com/authors/Gede_Primahadi_Wijaya_Rajeg/1234749)<br/> **License:** [GPL-2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)<br/>

[![Travis-CI Build Status](https://travis-ci.org/gederajeg/collogetr.svg?branch=master)](https://travis-ci.org/gederajeg/collogetr)<br/> [![Coverage Status](https://img.shields.io/codecov/c/github/gederajeg/collogetr/master.svg)](https://codecov.io/github/gederajeg/collogetr?branch=master)

collogetr
=========

Overview
--------

`collogetr` has one function (viz. `colloc_leipzig()`) to retrieve window-span collocates for a set of word forms (viz. the *node word*) from the (Indonesian) [Leipzig Corpora](http://wortschatz.uni-leipzig.de/en/download). There are two functions to process the output of `colloc_leipzig()` into tabular formats as input for **association measure** between the collocates and the node, as in the [collostructional/collocation analysis](http://www.linguistics.ucsb.edu/faculty/stgries/teaching/groningen/index.html). These functions are `assoc_prepare()` and `assoc_prepare_dca()`. The former generates input for [*Simple Collexeme/Collocational Analysis*](http://www.linguistics.ucsb.edu/faculty/stgries/research/2003_AS-STG_Collostructions_IJCL.pdf) computed using `collex_fye()`, meanwhile the latter uses the output of `assoc_prepare()` to generate input for [*Distinctive Collexeme/Collocates Analysis*](http://www.linguistics.ucsb.edu/faculty/stgries/research/2004_STG-AS_ExtendingCollostructions_IJCL.pdf) computed using `collex_fye_dca()`. The package is built on top of the core [tidyverse](https://www.tidyverse.org) packages.

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

``` r
citation("collogetr")
#> 
#> To cite package 'collogetr' in publications use:
#> 
#>   Gede Primahadi Wijaya Rajeg (2018). collogetr: Collocates
#>   retriever for the Indonesian Leipzig Corpora. R package version
#>   0.1.0. https://github.com/gederajeg/collogetr
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {collogetr: Collocates retriever for the Indonesian Leipzig Corpora},
#>     author = {Gede Primahadi Wijaya Rajeg},
#>     year = {2018},
#>     note = {R package version 0.1.0},
#>     url = {https://github.com/gederajeg/collogetr},
#>   }
```

### Package data

The package has three data sets for demonstration. The important one is the `demo_corpus_leipzig` whose documentation can be accessed via `?demo_corpus_leipzig`. Another data is a list of Indonesian stopwords (i.e. `stopwords`) that can be filtered out when performing collocational measure. The last one is `leipzig_corpus_path` containing character vector of full path to my Leipzig Corpus files in my computer.

#### Accepted inputs

`colloc_leipzig()` accepts two types of input data:

1.  A named-list object with character-vector elements of each Leipzig Corpus Files, represented by `demo_corpus_leipzig` and the format of which is shown below:

``` r
lapply(demo_corpus_leipzig[1:2], sample, 2)
#> $ind_mixed_2012_1M
#> [1] "154926 Dengan dipasangnya hotspot di titik-titik tertentu seorang peneliti bisa leluasa mencari informasi, membuka emial dan mengirim email sesuai keinginan."
#> [2] "673221 Juga ada gambar ke dua, komposisi mirip gambar pertama, bertuliskan 'NKRI' di sebelah kiri, tulisan 'Hijrah' di tengah, dan tulisan 'NII' di kanan."   
#> 
#> $ind_news_2008_300K
#> [1] "124531 Pada waktu itu, harga minyak selama 20 tahun stabil 10-20 dolar AS, sehingga patokan batas atas Fujian sebesar 25 dolar AS dinilai cukup layak."                                                                                      
#> [2] "249133 Angka penjualan kantor dan mal naik dua kali lipat pada semester I dari Rp26,9 miliar menjadi Rp62,3 miliar tahun 2008 berasal dari Bakrie Tower yang berlokasi di Rasuna Epicentrum super blok pertama yang dikembangkan Bakrieland."
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
#> Warning in colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
#> pattern = "mengatakan", : No match is detected for the input form
#> 'mengatakan' in ind_wikipedia_2016_1M.
```

In the example above, the collocates are restricted to those occurring *one* word (i.e. `span = 1L`) to the *right* (`window = "r"`) of *mengatakan* 'to say'. The `"r"` character in `window` stands for *right*-side collocates (`"l"` for *left*-side collocates and `"b"` for *both* right- and left-side collocates). The `span` argument requires integer (i.e., a whole number) to indicate the range of words covered in the specified window. The `pattern` argument requires one or more exact word forms; if more than one, put into a character vector (e.g., `c("mengatakan", "menjanjikan")`). The `pattern` argument also accepts strings marked with *word boundary* character (i.e. `"\\b"`), thus `c("\\bmengatakan\\b", "\\bmenjanjikan\\b")`.

The `save_interim` is `FALSE` means that no output is saved into the computer, but in the console (i.e., in the `out` object). If `save_interim = TRUE`, the function will save the outputs into the files in the computer. `colloc_leipzig()` has specified the default file names for the outputs via these arguments: (i) `freqlist_output_file`, (ii) `colloc_output_file`, (iii) `corpussize_output_file`, and (iv) `search_pattern_output_file`. It is recommended that the output filenames are stored as a character vector. See **Examples** "(2)" in the documentation of `colloc_leipzig()` for a call when `save_interim = TRUE`.

#### Exploring the output of `colloc_leipzig()`.

The output of `colloc_leipzig()` is a list of 4 elements:

1.  `colloc_df`; a table/tibble of raw collocates data with columns for:
    1.  corpus names
    2.  sentence id in which the collocates and the node word(s) are found
    3.  numeric vector of position of the collocates in the given sentences
    4.  the collocates (column `w`)
    5.  the span information (e.g., `"r1"` for one-word, right-side collocates)
    6.  the node word
    7.  the text/sentence match in which the collocates and the node are found
2.  `freqlist_df`; a table/tibble of word-frequency list in the loaded corpus
3.  `corpussize_df`; a table/tibble of total word-tokens in the loaded corpus
4.  `pattern`; a character vector of the search pattern/node

``` r
str(out)
#> List of 4
#>  $ colloc_df    :Classes 'tbl_df', 'tbl' and 'data.frame':   129 obs. of  8 variables:
#>   ..$ corpus_names : chr [1:129] "ind_mixed_2012_1M" "ind_mixed_2012_1M" "ind_mixed_2012_1M" "ind_mixed_2012_1M" ...
#>   ..$ sent_id      : int [1:129] 141 165 178 205 10 125 128 137 140 152 ...
#>   ..$ sent_elements: int [1:129] 21 4 7 4 13 4 4 16 4 7 ...
#>   ..$ w_vector_pos : int [1:129] 2277 2709 2960 3516 232 2619 2694 2845 2908 3125 ...
#>   ..$ w            : chr [1:129] "pengikut" "untuk" "firman" "dirinya" ...
#>   ..$ span         : chr [1:129] "r1" "r1" "r1" "r1" ...
#>   ..$ node         : chr [1:129] "mengatakan" "mengatakan" "mengatakan" "mengatakan" ...
#>   ..$ sent_match   : chr [1:129] "592813 ” Saya harap yg anda maksudkan pengikut adalah mereka yg mengikuti debat karena saya tidak punya pengiku"| __truncated__ "350347 Warsito mengatakan, untuk mengatasi jumlah pengangguran salah satunya adalah dengan jalan mengirimkan TK"| __truncated__ "372390 Ia berasal dari Allah, mengatakan firman Allah, dan memberitahukan masa depan, maka Ia adalah seorang na"| __truncated__ "575742 Ia mengatakan, dirinya telah menyampaikan kepada Hendi, pada prinsipnya provinsi bersedia membantu secar"| __truncated__ ...
#>  $ freqlist_df  :Classes 'tbl_df', 'tbl' and 'data.frame':   28247 obs. of  3 variables:
#>   ..$ corpus_names: chr [1:28247] "ind_mixed_2012_1M" "ind_mixed_2012_1M" "ind_mixed_2012_1M" "ind_mixed_2012_1M" ...
#>   ..$ w           : chr [1:28247] "yang" "dan" "di" "dengan" ...
#>   ..$ n           : int [1:28247] 106 105 75 50 41 37 35 35 33 31 ...
#>  $ corpussize_df:Classes 'tbl_df', 'tbl' and 'data.frame':   14 obs. of  2 variables:
#>   ..$ corpus_names: chr [1:14] "ind_mixed_2012_1M" "ind_news_2008_300K" "ind_news_2009_300K" "ind_news_2010_300K" ...
#>   ..$ size        : int [1:14] 3812 4537 4723 4671 4858 4816 3981 3935 4038 3963 ...
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
#> 1 pihaknya   mengatakan <tibble [1 × 9]>
#> 2 pemerintah mengatakan <tibble [1 × 9]>
#> 3 pelaku     mengatakan <tibble [1 × 9]>
#> 4 aksi       mengatakan <tibble [1 × 9]>
#> 5 anjloknya  mengatakan <tibble [1 × 9]>
#> 6 aparat     mengatakan <tibble [1 × 9]>
```

The `assoc_prepare()` and `collex_fye()` functions are designed following the tidy principle so that the association/collocation measure is performed in a row-wise fashion, benefiting from the combination of [*nested* column](http://r4ds.had.co.nz/many-models.html#list-columns-1) for the input-data (using `tidyr::nest()`) and `purrr`'s `map_*` function. `assoc_prepare()` includes calculating the expected co-occurrence frequencies between the collocates/collexemes and the node word/construction.

The column `data` in `assoc_tb` above consists of nested tibble/table as a list. Each contains required data for performing association measure for each of the collocates in column `w`. This nested column can be inspected as follows (for the first row, namely for the word *pihaknya* 'the party').

``` r
# get the tibble in the `data` column for the first row
assoc_tb$data[[1]]
#> # A tibble: 1 x 9
#>   a_exp     a n_w_in_corp corpus_size n_pattern     b     c     d assoc   
#>   <dbl> <int>       <int>       <int>     <int> <int> <int> <int> <chr>   
#> 1 0.102     7          31       39133       129    24   122 38980 attract…
```

Column `a` indicates the co-occurrence frequency between the node word and the collocates column `w`, meanwhile `a_exp` indicates the *expected co-occurrence frequency* between them. The `n_w_in_corp` represents the total token/occurrence frequency of a given collocate. The `n_pattern` stores the total token/occurrence frequency of the node word in the corpus. Column `b`, `c`, and `d` are required for the association measure that is essentially based on 2-by-2 crosstabulation table. The `assoc` column indicates whether the value in `a` is higher than that in `a_exp`, thus indicating *attraction* or *positive association* between the node word and the collocate. The reverse is *repulsion* or *negative association* when the value in `a` is less/lower than that in `a_exp`.

#### *Simple Collexeme/Collocates Analysis (SCA)*

As in the *Collostructional Analysis* (cf. Stefanowitsch and Gries, [2003](http://www.linguistics.ucsb.edu/faculty/stgries/research/2003_AS-STG_Collostructions_IJCL.pdf)), `collex_fye()` uses one-tailed *Fisher-Yates Exact* test whose *p*-<sub>FisherExact</sub>value is log-transformed to the base of 10 to indicate the collostruction strength between the collocates and the node word (cf., e.g., Gries, Hampe, and Schönefeld, [2005](http://www.linguistics.ucsb.edu/faculty/stgries/research/2005_STG-BH-DS_CollStr-vs-Freq_CogLing.pdf), *inter alia*). `collex_fye()` simultaneously performs two uni-directional measures of *Delta P*; one of these shows the extent to which the presence of the node-word cues the collocates/collexemes, and *vice versa*.

Here is the code to perform the SCA

``` r
# perform FYE test for Collexeme Analysis
am_fye <- collex_fye(df = assoc_tb, collstr_digit = 3)
```

Now we can retrieve the top-10 most strongly attracted collocates to *mengatakan* 'to say sth.'. The association strength is shown in the `collstr` column, which stands for *collostruction strength*. The higher, the stronger the association.

``` r
# get the top-10 most strongly attracted collocates
dplyr::top_n(am_fye, 10, collstr)
#> # A tibble: 10 x 9
#>    w       node        a   a_exp assoc      p_fye collstr dP_collex_cue_c…
#>    <chr>   <chr>   <int>   <dbl> <chr>      <dbl>   <dbl>            <dbl>
#>  1 pihakn… mengat…     7 0.102   attra…  8.83e⁻¹²   11.1           0.0540 
#>  2 anjlok… mengat…     1 0.00300 attra…  3.30e⁻ ³    2.48          0.00800
#>  3 chas    mengat…     1 0.00300 attra…  3.30e⁻ ³    2.48          0.00800
#>  4 kenaik… mengat…     1 0.00300 attra…  3.30e⁻ ³    2.48          0.00800
#>  5 pengga… mengat…     1 0.00300 attra…  3.30e⁻ ³    2.48          0.00800
#>  6 penyad… mengat…     1 0.00300 attra…  3.30e⁻ ³    2.48          0.00800
#>  7 pfizer  mengat…     1 0.00300 attra…  3.30e⁻ ³    2.48          0.00800
#>  8 provok… mengat…     1 0.00300 attra…  3.30e⁻ ³    2.48          0.00800
#>  9 unsrat  mengat…     1 0.00300 attra…  3.30e⁻ ³    2.48          0.00800
#> 10 pelaku  mengat…     2 0.0860  attra…  3.33e⁻ ³    2.48          0.0150 
#> # ... with 1 more variable: dP_cxn_cue_collex <dbl>
```

Column `a` contains the co-occurrence frequency of the collocates (`w`) with the `node` as its R1 collocates in the demo corpus. `p_fye` shows the one-tailed *p*<sub>FisherExact</sub>-value. Updated README file is prepared for retrieving data to perform *Distinctive Collexeme Analysis*.
