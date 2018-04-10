
<!-- README.md is generated from README.Rmd. Please edit that file -->
**Authors:** [Gede Primahadi Wijaya Rajeg](https://figshare.com/authors/Gede_Primahadi_Wijaya_Rajeg/1234749)<br/> **License:** [GPL-2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)<br/>

[![Travis-CI Build Status](https://travis-ci.com/gederajeg/collogetr.svg?branch=master)](https://travis-ci.com/gederajeg/collogetr)<br/> [![Coverage Status](https://img.shields.io/codecov/c/github/gederajeg/collogetr/master.svg)](https://codecov.io/github/gederajeg/collogetr?branch=master)

collogetr
=========

The goal of collogetr is to perform window-span collocates retrieval from the sentence-based corpus of the (Indonesian) [Leipzig Corpora](http://wortschatz.uni-leipzig.de/en/download). It is powered by most of the core packages from the [tidyverse](https://www.tidyverse.org). The initial purpose of this package was to help me with my [PhD thesis](http://rpubs.com/primahadi/ca_3d_metaphor_happiness_synonyms_Indonesian) on <span style="font-variant:small-caps;">happiness</span> metaphors in Indonesian based on data from the Leipzig Corpora.

This package is under development. There is currently one function (`colloc_leipzig()`) available. Future plan is to include other function for generating non-Leipzig, sentence-based corpus inputs, and functions to perform [collostructional/collocation analysis](http://www.linguistics.ucsb.edu/faculty/stgries/teaching/groningen/index.html). The development version of the package can be installed via GitHub with [devtools](https://github.com/hadley/devtools):

``` r
library(devtools)
install_github("gederajeg/collogetr")
```

Example for retrieving the collocates with `colloc_leipzig()`.
--------------------------------------------------------------

The following code shows how to use `colloc_leipzig()` to search for the collocates for the directional preposition *ke* 'to' in Indonesian. The function will print out progress messages for steps taken. The input corpus below (i.e. `demo_corpus_leipzig`) is included as data in this package whose documentation can be accessed via `?demo_corpus_leipzig`.

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

The collocates are restricted to those occurring one-word to the right of *ke* (i.e., its R1 collocates). The window direction and span are respectively specified in the `window` and `span` arguments. The `"r"` character in `window` stands for 'right'-side collocates (`"l"` for 'left'-side collocates and `"b"` for both right- and left-side collocates). The `span` argument requires integer to indicate the range of words covered in the specified window. The `pattern` argument requires regular expression input. `colloc_leipzig()` accept two kinds of corpus-input. First, a named-list object with character-vector elements of each Leipzig Corpus Files. The format of this kind of input is shown below. Note that the `demo_corpus_leipzig` is included as dataset in this package.

``` r
lapply(demo_corpus_leipzig[2:3], sample, 3)
#> $ind_news_2008_300K
#> [1] "159057 Oleh karena itu, pihaknya atas nama pimpinan ITS mengucapkan terima kasih yang sebesar-besarnya kepada segenap masyarakat ITS yang telah ikut membantu selama proses visitasi dengan tulus."
#> [2] "124374 \"Putusan PTUN mengacu pada putusan Mahkamah Konstitusi (MK)."                                                                                                                              
#> [3] "224221 Billy yang membawa tas warna hitam berdiri di sebelah Iqbal."                                                                                                                               
#> 
#> $ind_news_2009_300K
#> [1] "97227 Berbicara singkat dua menit sebelum acara konser yang menampilkan operet dan lagu dari grup legendaris The Beatles, Peggy memperkenalkan Indonesia sebagai negeri ribuan pulau dan penduduk 200 juta, kemudian mengajak anak-anak itu mengunjungi Indonesia."
#> [2] "28974 Perbaikan tersebut diharuskan, mengingat dalam rapat pleno KPU diketahui bahwa masih ada masalah pada penghitungan perolehan suara di Nias Selatan."                                                                                                         
#> [3] "82265 Rencananya lagu ini akan dibikin album bersama Nina Tamam, Rika Roeslan dan Iga Mawarni."
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

The first is `collocs_df` that is a tibble of raw collocates data, that is, not yet counted for their frequencies. The frequencies of the collocates are stored in the second element, namely `collocs_freq`. The `words_freq` element consist of frequency list of all word-tokens in the loaded corpus. This frequencly-list data (and the `all_corpus_size` data), is included for further development of this function to include functions to perform collocational strength measure for the search pattern with the collocates.

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
