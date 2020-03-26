context("coll_sentmatch_tagged.R")

collout <- collogetr::colloc_leipzig(leipzig_corpus_list = collogetr::demo_corpus_leipzig,
                                     pattern = "sudah",
                                     window = "b",
                                     span = 2,
                                     save_interim = FALSE)

df_sentmatch <- collogetr::colloc_sentmatch_tagged(collout, colloc = "ada")

testthat::test_that("colloc_sentmatch_tagged generates a data frame", {
  testthat::expect_output(str(df_sentmatch), "tbl_df")
})

collout <- collogetr::colloc_leipzig(leipzig_corpus_list = collogetr::demo_corpus_leipzig,
                                     pattern = "sebesar",
                                     window = "b",
                                     span = 3,
                                     save_interim = FALSE)

df_sentmatch <- collogetr::colloc_sentmatch_tagged(collout, colloc = "persen")
testthat::test_that("colloc_sentmatch_tagged generates a data frame", {
  testthat::expect_length(df_sentmatch, 7)
})

