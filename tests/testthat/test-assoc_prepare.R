context("test-assoc_prepare.R")

# run the colloc_leipzig
out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig[1:4],
                      pattern = "\\bke\\b",
                      window = "r",
                      span = 1,
                      save_interim = FALSE)

# run assoc_prepare
assoc_tb <- assoc_prepare(out)

test_that("output of assoc_prepare is a tibble", {
  expect_output(str(assoc_tb), "(tbl_df|tbl|data\\.frame)", perl = TRUE)
})

test_that("output of assoc_prepare consists of eight (8) column", {
  expect_equal(dim(assoc_tb)[2], 8L)
})

# prepare small stopwords list
stopwords_list <- c("yang", "akan", "akankah", "melalui", "dari", "adalah", "ketika", "dan", "atau")

test_that("assoc_prepare produces messages for removal/retention of stopwords", {
  expect_message(assoc_prepare(out, stopword_list = stopwords_list), "^You chose to", perl = TRUE)
  expect_message(assoc_prepare(out, stopword_list = NULL), "are retained\\!$", perl = TRUE)
})

coll_freq <- out$collocs_freq
word_freq <- out$words_freq
coll_word_freq <- dplyr::inner_join(coll_freq, word_freq, by = "w")

test_that("inner_join of word_freq with coll_freq results in the same dimension with coll_freq", {
  expect_equal(dim(coll_freq)[1], dim(coll_word_freq)[1])
})



