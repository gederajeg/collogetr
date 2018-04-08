# load stopwords
stopwords1 <- "/Users/Primahadi/Downloads/stopwords_id.txt"
stopwords2 <- "/Users/Primahadi/Downloads/stopword_id_new.txt"

stopwords1 <- readr::read_lines(stopwords1, skip = 2)
stopwords2 <- readr::read_lines(stopwords2)
stopwords <- unique(c(stopwords1, stopwords2))

# store the full path of the folder containing the leipzig corpora
leipzig_corpus_directory <- "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora"
leipzig_wordlist_directory <- "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora/leipzig_wordlist"

# orti_bali_corpus_path
orti_bali_folder <- "/Users/Primahadi/Documents/Corpora/_corpusbali/Surat Kabar/Orti Bali/utf-8 version"
orti_bali_path <- dir(path = orti_bali_folder, pattern = "\\.txt$", full.names = TRUE)
Encoding(orti_bali_path) <- "UTF-8"

# get the complete path to each file of the leipzig corpora
# by indexing (in the "pattern" argument) the initials of the corpus files (i.e. "^ind(_|[-])")
leipzig_corpus_name <- dir(path = leipzig_corpus_directory,
                           pattern = "^ind(_|[-])",
                           full.names = FALSE) # only the basename ("full.names=FALSE")
names(leipzig_corpus_name) <- 1:length(leipzig_corpus_name)

leipzig_corpus_path <- dir(path = leipzig_corpus_directory,
                           pattern = "^ind(_|[-])",
                           full.names = TRUE) # full path ("full.names=TRUE")
leipzig_corpus_id <- stringr::str_replace(leipzig_corpus_name, "-sentences.*$", "")
names(leipzig_corpus_id) <- 1:length(leipzig_corpus_id)

leipzig_vector_path <- dir(path = leipzig_corpus_directory,
                           pattern = "^corpus_word_vector__",
                           full.names = TRUE)
leipzig_vector_name <- stringr::str_replace(basename(leipzig_vector_path), "^corpus_.+?__", "")

leipzig_cleaned_path <- dir(path = leipzig_corpus_directory,
                            pattern = "^corpus_sent_vector__",
                            full.names = TRUE)
leipzig_cleaned_name <- stringr::str_replace(basename(leipzig_cleaned_path), "^corpus_.+?__", "")

# get the complete path to each file of the leipzig corpora wordlist
leipzig_wordlist_path <- dir(path = leipzig_wordlist_directory, full.names = TRUE)
leipzig_wordlist_name <- stringr::str_replace(basename(leipzig_wordlist_path), "-words.*$", "")
leipzig_mywordlist_path <- dir(path = leipzig_corpus_directory, pattern = "_wordlist__.+?\\.txt$", full.names = TRUE)
leipzig_mywordlist_name <- stringr::str_replace_all(basename(leipzig_mywordlist_path), "(^corpus_.+?__(?=ind)|\\.txt$)", "")

# generate internal data
devtools::use_data(leipzig_corpus_name,
                   leipzig_corpus_path,
                   orti_bali_path,
                   leipzig_vector_path,
                   leipzig_vector_name,
                   leipzig_cleaned_path,
                   leipzig_cleaned_name,
                   #leipzig_corpus_id,
                   #leipzig_corpus_directory,
                   #leipzig_wordlist_directory,
                   leipzig_wordlist_name,
                   leipzig_wordlist_path,
                   leipzig_mywordlist_path,
                   leipzig_mywordlist_name,
                   stopwords,
                   stopwords1,
                   stopwords2,
                   internal = TRUE,
                   overwrite = TRUE)

# generate external data
devtools::use_data(leipzig_corpus_path,
                   # orti_bali_path,
                   # leipzig_vector_path,
                   # leipzig_cleaned_path,
                   # leipzig_mywordlist_path,
                   stopwords,
                   internal = FALSE,
                   overwrite = TRUE)
