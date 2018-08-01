context("test-assoc_prepare.R")

# run the colloc_leipzig
out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig[1:4],
                      pattern = "mengatakan",
                      window = "b",
                      span = 3L,
                      save_interim = FALSE)

# run assoc_prepare
assoc_tb <- assoc_prepare(out)

test_that("output of assoc_prepare is a tibble", {
  expect_output(str(assoc_tb), "(tbl_df|tbl|data\\.frame)", perl = TRUE)
})

test_that("output of assoc_prepare consists of three (3L) columns when `per_corpus` is `FALSE`", {
  expect_equal(dim(assoc_tb)[2], 3L)
  expect_output(str(assoc_prepare(out)), "(3 variables|\\$ (w|node|data))")
})

test_that("selected window-span reduces the number of cases in the output", {
  expect_true(dim(assoc_prepare(out, window_span = c("r1", "r2")))[1] < dim(assoc_prepare(out, window_span = NULL))[1])
  expect_true(dim(assoc_prepare(out, window_span = c("r1", "r2", "r3")))[1] < dim(assoc_prepare(out, window_span = NULL))[1])
  expect_true(dim(assoc_prepare(out, window_span = c("l1", "l2", "l3")))[1] < dim(assoc_prepare(out, window_span = NULL))[1])
})

test_that("output of assoc_prepare consists of three (4L) columns when `per_corpus` is `TRUE`", {
  expect_equal(dim(assoc_prepare(out, per_corpus = TRUE))[2], 4L)
  expect_output(str(assoc_prepare(out, per_corpus = TRUE)), "(4 variables|\\$ (corpus_names|w|node|data))")
})


# prepare small stopwords list
stopwords_list <- c("yang", "akan", "akankah", "melalui", "dari", "adalah", "ketika", "dan", "atau")

test_that("assoc_prepare produces messages for removal/retention of stopwords", {
  expect_message(assoc_prepare(out, stopword_list = stopwords_list), "^You chose to", perl = TRUE)
  expect_message(assoc_prepare(out, stopword_list = NULL), "are retained\\!$", perl = TRUE)
})




