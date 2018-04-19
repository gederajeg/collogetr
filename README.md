
<!-- README.md is generated from README.Rmd. Please edit that file -->
**Authors:** [Gede Primahadi Wijaya Rajeg](https://figshare.com/authors/Gede_Primahadi_Wijaya_Rajeg/1234749)<br/> **License:** [GPL-2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)<br/>

[![Travis-CI Build Status](https://travis-ci.org/gederajeg/collogetr.svg?branch=master)](https://travis-ci.org/gederajeg/collogetr)<br/> [![Coverage Status](https://img.shields.io/codecov/c/github/gederajeg/collogetr/master.svg)](https://codecov.io/github/gederajeg/collogetr?branch=master)

collogetr
=========

The goal of collogetr is to perform window-span collocates retrieval from the sentence-based corpus of the (Indonesian) [Leipzig Corpora](http://wortschatz.uni-leipzig.de/en/download). It is powered by most of the core packages from the [tidyverse](https://www.tidyverse.org). The initial purpose of this package was to help me with my [PhD thesis](http://rpubs.com/primahadi/ca_3d_metaphor_happiness_synonyms_Indonesian) on <span style="font-variant:small-caps;">happiness</span> metaphors in Indonesian based on data from the Leipzig Corpora.

This package is under development. Currently, there is one function available for retrieving the collocates (i.e., `colloc_leipzig()`) and two other functions to prepare data (i.e., `assoc_prepare()`) and then perform association measure of collocates with the node-word as in the [collostructional/collocation analysis](http://www.linguistics.ucsb.edu/faculty/stgries/teaching/groningen/index.html) (i.e., `collex_fye()`). Future plan is to include other function for generating non-Leipzig, sentence-based corpus inputs. The development version of the package can be installed via GitHub with [devtools](https://github.com/hadley/devtools):

``` r
library(devtools)
install_github("gederajeg/collogetr")
```

Example for retrieving the collocates with `colloc_leipzig()`.
--------------------------------------------------------------

The following code shows how to use `colloc_leipzig()` to search for the collocates for the future marker *ke* 'to' in Indonesian. The function will print out progress messages for steps taken. The input corpus below (i.e. `demo_corpus_leipzig`) is included as data in this package whose documentation can be accessed via `?demo_corpus_leipzig`.

``` r
library(collogetr)
out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig[2:3],
                       pattern = "\\bke\\b",
                       window = "r",
                       span = 1,
                       save_interim = FALSE)
#> Detecting a "list" input of corpora!
#> Processing "ind_news_2008_300K"...
#> 17 matches are detected!
#> 1. Tokenising the corpus into corpus word-vector...
#>     1.1 Renaming the tokens with sentence and corpus labels...
#>     1.2 Vectorising the tokens...
#>     1.3 Lowercasing the word-tokens...
#>     1.4 Removing one-letter tokens...
#> 2. Gathering the vector-position of the search pattern...
#> 3. Gathering the collocates of the search pattern...
#> 4. Storing the output...
#> DONE with "ind_news_2008_300K"!
#> Detecting a "list" input of corpora!
#> Processing "ind_news_2009_300K"...
#> 16 matches are detected!
#> 1. Tokenising the corpus into corpus word-vector...
#>     1.1 Renaming the tokens with sentence and corpus labels...
#>     1.2 Vectorising the tokens...
#>     1.3 Lowercasing the word-tokens...
#>     1.4 Removing one-letter tokens...
#> 2. Gathering the vector-position of the search pattern...
#> 3. Gathering the collocates of the search pattern...
#> 4. Storing the output...
#> DONE with "ind_news_2009_300K"!
```

The collocates are restricted to those occurring one-word to the right of *ke* (i.e., its R1 collocates). The window direction and span are respectively specified in the `window` and `span` arguments. The `"r"` character in `window` stands for 'right'-side collocates (`"l"` for 'left'-side collocates and `"b"` for both right- and left-side collocates). The `span` argument requires integer to indicate the range of words covered in the specified window. The `pattern` argument requires regular expression input. `colloc_leipzig()` accept two kinds of corpus-input. First, a named-list object with character-vector elements of each Leipzig Corpus Files. The format of this kind of input is shown below.

``` r
lapply(demo_corpus_leipzig[2:3], sample, 3)
#> $ind_news_2008_300K
#> [1] "251233 Dia menyatakan, Slank akan terus berkarya melalui musik yang memberikan pesan moral."                                                                               
#> [2] "96096 \"Kami ingin mengatasi kemiskinan pada sumbernya, kami harus membantu wanita menghindari kehamilan yang tak diinginkan, kata sekretaris yayasan."                    
#> [3] "72273 Setiap tim yang dapat menang 2-1 di Roma merupakan lawan berbaya dalam setiap kompetisi,\" kata Scolari dua minggu lalu setelah ia mendengar hasil pertandingan itu."
#> 
#> $ind_news_2009_300K
#> [1] "184393 Ia mengatakan dirinya mengalami luka pada kaki dan kepalanya karena tertimpa reruntuhan atap rumah."                                                             
#> [2] "188663 \"Mereka dimintai keterangan sebagai saksi,\" kata Juru Bicara KPK Johan Budi di Jakarta, Kamis."                                                                
#> [3] "112108 Bentuk kerjasama ini, kata Subekti, juga sangat penting sebagai upaya Indonesia untuk mengurangi ketergantungan negara-negara barat di bidang teknologi militer."
```

The second input is full-path to the Leipzig Corpus Files saved as UTF-8 encoded plain-texts. If this kind of input is preferred, supply the path to the `leipzig_path` argument; the `leipzig_corpus_list` will be by default set with `NULL`.

Exploring the output of `colloc_leipzig()`.
-------------------------------------------

The output of `colloc_leipzig()` is a list of 6 elements.

``` r
str(out)
#> List of 6
#>  $ collocs_df      :Classes 'tbl_df', 'tbl' and 'data.frame':    38 obs. of  3 variables:
#>   ..$ corpus_names: chr [1:38] "ind_news_2008_300K" "ind_news_2008_300K" "ind_news_2008_300K" "ind_news_2008_300K" ...
#>   ..$ sent_id     : chr [1:38] "1" "2" "2" "7" ...
#>   ..$ w           : chr [1:38] "gapensi" "posisi" "level" "lokasi" ...
#>  $ collocs_freq    :Classes 'tbl_df', 'tbl' and 'data.frame':    36 obs. of  2 variables:
#>   ..$ w: chr [1:36] "dalam" "tempat" "belakang" "bentuk" ...
#>   ..$ a: int [1:36] 2 2 1 1 1 1 1 1 1 1 ...
#>  $ words_freq      :Classes 'tbl_df', 'tbl' and 'data.frame':    3328 obs. of  2 variables:
#>   ..$ w          : chr [1:3328] "yang" "dan" "di" "itu" ...
#>   ..$ n_w_in_corp: int [1:3328] 261 200 184 133 91 89 86 83 81 79 ...
#>  $ all_corpus_size : int 9260
#>  $ node_regex      :Classes 'regex', 'pattern', 'character'  atomic [1:1] \bke\b
#>   .. ..- attr(*, "options")=List of 4
#>   .. .. ..$ case_insensitive: logi TRUE
#>   .. .. ..$ comments        : logi FALSE
#>   .. .. ..$ dotall          : logi FALSE
#>   .. .. ..$ multiline       : logi FALSE
#>  $ node_regex_exact: chr "^ke$"
```

The first is `collocs_df` that is a tibble of raw collocates data, that is, not yet counted for their frequencies. The frequencies of the collocates are stored in the second element, namely `collocs_freq`. The `words_freq` element consist of frequency list of all word-tokens in the loaded corpus. This frequencly-list data (and the `all_corpus_size` data), is included for the development of this function to include functions to perform collocational strength measure for the search pattern with the collocates (i.e., `collex_fye()`; cf. below).

``` r
# top-10 most frequent collocates in the sample corpus
out$collocs_freq[1:10,]
#> # A tibble: 10 x 2
#>    w            a
#>    <chr>    <int>
#>  1 dalam        2
#>  2 tempat       2
#>  3 belakang     1
#>  4 bentuk       1
#>  5 berat        1
#>  6 daerah       1
#>  7 depan        1
#>  8 desa         1
#>  9 dpr          1
#> 10 gapensi      1
```

Performing association measure (i.e., *Collexeme Analysis*) with `collex_fye()`.
--------------------------------------------------------------------------------

There are two available functions for the purpose of *Collexeme Analysis* with this package. The first one is `assoc_prepare()`. This function prepares the data (i.e., the output of `colloc_leipzig()`) into a tidy format required to perform the association measure (with `collex_fye()`). As in the Collostructional Analysis (cf. Stefanowitsch and Gries, [2003](http://www.linguistics.ucsb.edu/faculty/stgries/research/2003_AS-STG_Collostructions_IJCL.pdf)), the measure uses one-tailed *Fisher-Yates Exact* test whose *p*-<sub>fisher</sub>value is log-transformed to the base of 10 to indicate the collostruction strength between the collocates and the node word/construction (cf., e.g., Gries, Hampe, and Schönefeld, [2005](http://www.linguistics.ucsb.edu/faculty/stgries/research/2005_STG-BH-DS_CollStr-vs-Freq_CogLing.pdf), *inter alia*). `collex_fye()` simultaneously performs two uni-directional measures of *Delta P*, one in which the extent to which the presence of the node-word/construction cues the collocates/collexemes, and *vice versa*.

These two functions are designed following the tidy principle so that the association measure is performed in a row-wise fashion, benefiting from the combination of [*nested* column](http://r4ds.had.co.nz/many-models.html#list-columns-1) for the input-data (using `tidyr::nest()`) and `purrr`'s `map_*` function.

The following chunk illustrates the retrieval of right-side collocates of future marker *akan* 'will/be going to' in Indonesian and the use of `assoc_prepare()` to generate the input-data for the association measure of the collocates with *akan*.

``` r
# search the collocates
out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
                       pattern = "\\bakan\\b",
                       window = "r",
                       span = 3,
                       save_interim = FALSE)

# prepare the input data; stopwords list are included in the package
assoc_tb <- assoc_prepare(colloc_out = out, stopword_list = stopwords)

# peek into the assoc_tb
head(assoc_tb, 3)
#> # A tibble: 3 x 2
#>   w       data            
#>   <chr>   <list>          
#> 1 menjadi <tibble [1 × 9]>
#> 2 terus   <tibble [1 × 9]>
#> 3 membuat <tibble [1 × 9]>
```

The column `data` in `assoc_tb` consists of nested tibble/table as a list. Each contains required data for performing association measure for each of the collocates in column `w`. This nested column can be inspected as follows (for the first row, i.e. for the word *menjadi* 'to become').

``` r
# get the tibble in the `data` column for the first row
assoc_tb$data[[1]]
#> # A tibble: 1 x 9
#>       a n_w_in_corp corpus_size n_pattern     b     c     d a_exp assoc   
#>   <int>       <int>       <int>     <int> <int> <int> <int> <dbl> <chr>   
#> 1    12         223       41962       429   211   417 41322  2.28 attract…
```

Then, we can perform the Collexeme Analysis as follows with `assoc_tb` as the input for the `df` argument. The output is sorted in descending order of the Collostruction Strength (in the `collstr` column).

``` r
# perform FYE test for Collexeme Analysis
am_fye <- collex_fye(df = assoc_tb, collstr_digit = 3)

# get the top-15 most strongly attracted collocates
dplyr::top_n(am_fye, 15, collstr)
#> # A tibble: 17 x 7
#>    w            a  a_exp assoc   collstr dP_collex_cue_c… dP_cxn_cue_coll…
#>    <chr>    <int>  <dbl> <chr>     <dbl>            <dbl>            <dbl>
#>  1 terus       11 0.450  attrac…   12.2           0.0250            0.240 
#>  2 menjadi     12 2.28   attrac…    5.42          0.0230            0.0440
#>  3 diumumk…     3 0.0409 attrac…    5.38          0.00700           0.740 
#>  4 membuat      7 0.695  attrac…    5.20          0.0150            0.0930
#>  5 mendapa…     6 0.521  attrac…    4.87          0.0130            0.108 
#>  6 datang       5 0.327  attrac…    4.76          0.0110            0.146 
#>  7 diketah…     4 0.164  attrac…    4.75          0.00900           0.240 
#>  8 diisi        3 0.0613 attrac…    4.68          0.00700           0.490 
#>  9 bersaing     3 0.0716 attrac…    4.44          0.00700           0.418 
#> 10 berlang…     4 0.194  attrac…    4.43          0.00900           0.200 
#> 11 berjalan     4 0.204  attrac…    4.34          0.00900           0.190 
#> 12 sulit        4 0.204  attrac…    4.34          0.00900           0.190 
#> 13 memberi…     6 0.654  attrac…    4.30          0.0130            0.0840
#> 14 menggun…     5 0.419  attrac…    4.22          0.0110            0.112 
#> 15 memetak…     2 0.0204 attrac…    3.98          0.00500           0.990 
#> 16 menghid…     2 0.0204 attrac…    3.98          0.00500           0.990 
#> 17 menindak     2 0.0204 attrac…    3.98          0.00500           0.990
```

The following code can be used to retrieved collocates that occur less frequently than expected (or in *repulsion* association).

``` r
dplyr::filter(am_fye, assoc == "repulsion")
#> # A tibble: 8 x 7
#>   w              a a_exp assoc   collstr dP_collex_cue_c… dP_cxn_cue_coll…
#>   <chr>      <int> <dbl> <chr>     <dbl>            <dbl>            <dbl>
#> 1 pemerintah     1  1.01 repuls…  -0.136          0                0      
#> 2 masyarakat     1  1.04 repuls…  -0.143          0                0      
#> 3 dua            1  1.16 repuls…  -0.168          0               -0.00100
#> 4 katanya        1  1.36 repuls…  -0.218         -0.00100         -0.00300
#> 5 satu           1  1.48 repuls…  -0.250         -0.00100         -0.00300
#> 6 indonesia      1  2.03 repuls…  -0.404         -0.00200         -0.00500
#> 7 orang          1  2.26 repuls…  -0.471         -0.00300         -0.00600
#> 8 tahun          1  2.33 repuls…  -0.493         -0.00300         -0.00600
```

Future development is to include a function to perform *Distinctive Collocates/Collexemes* analysis (cf. Gries and Stefanowitsch, [2004](http://www.linguistics.ucsb.edu/faculty/stgries/research/2004_STG-AS_ExtendingCollostructions_IJCL.pdf)).
