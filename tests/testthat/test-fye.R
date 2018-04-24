context("test-fye.R")

# prepare data
out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
                                            pattern = "\\bakan\\b",
                                            window = "r",
                                            span = 3,
                                            save_interim = FALSE)
assoc_tb <- assoc_prepare(colloc_out = out, stopword_list = stopwords)
am_fye <- collex_fye(df = assoc_tb, collstr_digit = 3)

test_that("collex_fye produces tibble", {
  expect_output(str(am_fye), "tbl_df")
})

test_that("collex_fye produces tibble of 7 columns", {
  expect_equal(dim(am_fye)[2], 7L)
})

test_that("collstr and the dPs measures are of the type 'double'", {
  expect_type(am_fye$collstr, "double")
  expect_type(am_fye$dP_collex_cue_cxn, "double")
  expect_type(am_fye$dP_cxn_cue_collex, "double")
})

collstr_vals <- c(Inf, 12.2, 5.42)
test_that("'Inf' and non-infinite collstr are of the type 'double'", {
  expect_type(collstr_vals, "double")
})
