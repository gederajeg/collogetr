context("test-fye.R")

# prepare data
out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
                                            pattern = "\\bakan\\b",
                                            window = "r",
                                            span = 3,
                                            save_interim = FALSE)
assoc_tb <- assoc_prepare(colloc_out = out, stopword_list = stopwords)
am_fye <- collex_fye(df = assoc_tb, collstr_digit = 3)

test_that("collex_fye produces tibble of 7 columns", {
  expect_equal(dim(am_fye)[2], 7L)
})
