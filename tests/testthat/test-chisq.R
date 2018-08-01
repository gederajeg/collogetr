context("test-chisq.R")

# prepare data
out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
                      pattern = "mengatakan",
                      window = "r",
                      span = 3L,
                      save_interim = FALSE)
assoc_tb <- assoc_prepare(colloc_out = out, stopword_list = stopwords)
am_chisq <- collex_chisq(assoc_tb)

test_that("collex_chisq produces tibble", {
  expect_output(str(am_chisq), "tbl_df")
})

test_that("collex_chisq produces tibble of 5 columns", {
  expect_equal(dim(collex_chisq(assoc_tb, collstr_digit = NULL))[2], 5L)
})

test_that("chi-square contribution is of the type 'double'", {
  expect_type(am_chisq$chisq, "double")
})
