context("test-collex_llr.R")

# prepare data
out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
                      pattern = "mengatakan",
                      window = "r",
                      span = 3L,
                      save_interim = FALSE)
assoc_tb <- assoc_prepare(colloc_out = out, stopword_list = stopwords)
am_llr <- collex_llr(df = assoc_tb, collstr_digit = 3)

test_that("collex_llr produces tibble", {
  expect_output(str(am_llr), "tbl_df")
})
