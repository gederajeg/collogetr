context("Mutual Information")

out <- collogetr::colloc_leipzig(leipzig_corpus_list = collogetr::demo_corpus_leipzig,
                                 pattern = "ke",
                                 window = "r",
                                 span = 1L,
                                 save_interim = FALSE)
assoc_tb <- collogetr::assoc_prepare(out, stopword_list = collogetr::stopwords[collogetr::stopwords != "ke"])

testthat::test_that("collex_MI produces a tibble", {

  testthat::expect_output(str(collogetr::collex_MI(assoc_tb)), "tbl_df")

})
