context("coll_sentmatch")

collout <- collogetr::colloc_leipzig(leipzig_corpus_list = collogetr::demo_corpus_leipzig,
                                     pattern = c("sudah", "akan"),
                                     window = "b",
                                     span = 2,
                                     save_interim = FALSE)
testthat::test_that("colloc_sentmatch throws error when `colloc` is not specified", {
  testthat::expect_error(collogetr::colloc_sentmatch(collout, colloc = NULL, wspan = NULL, nodeword = NULL, sampled = NULL),
                         "Specify with one or more collocates")
})

testthat::test_that("colloc_sentmatch returns a character vector", {
  testthat::expect_vector(collogetr::colloc_sentmatch(collout, colloc = "ada", wspan = NULL, nodeword = NULL, sampled = NULL), ptype = character())
  testthat::expect_vector(collogetr::colloc_sentmatch(collout, colloc = "ada", wspan = NULL, nodeword = "akan", sampled = NULL), ptype = character())
  testthat::expect_vector(collogetr::colloc_sentmatch(collout, colloc = "menjadi", wspan = NULL, nodeword = "akan", sampled = NULL), ptype = character())
  testthat::expect_vector(collogetr::colloc_sentmatch(collout, colloc = "menjadi", wspan = "r1", nodeword = NULL, sampled = NULL), ptype = character())
  testthat::expect_vector(collogetr::colloc_sentmatch(collout, colloc = "menjadi", wspan = "r1", nodeword = c("sudah", "akan"), sampled = 5), ptype = character())
})

testthat::test_that("colloc_sentmatch throws message when length of the sampled sentences are greater than the original output", {
  testthat::expect_warning(collogetr::colloc_sentmatch(collout, colloc = "menjadi", wspan = "r1", nodeword = c("sudah", "akan"), sampled = 50),
                           regexp = "Returning")
})

