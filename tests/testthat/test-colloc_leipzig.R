context("test-colloc_leipzig.R")

out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig[1:4],
                      pattern = "mengatakan",
                      window = "r",
                      span = 3L,
                      save_interim = FALSE)

testthat::test_that("output of colloc_leipzig is a list of four elements", {
  expect_output(str(out), "List of 4")
})


testthat::test_that("non-matching pattern of colloc_leipzig produces message", {
  expect_warning(colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig[1:2],
                                leipzig_path = NULL,
                                pattern = "memilikihgjdgjdsnvjsvnsj",
                                window = "r",
                                span = 1L,
                                save_interim = FALSE), regexp = "^No match is detected", perl = TRUE)
})

testthat::test_that("NULL corpus inputs and NULL pattern input produce error", {
  expect_error(colloc_leipzig(leipzig_corpus_list = NULL,
                              leipzig_path = NULL,
                              pattern = NULL,
                              window = "r",
                              span = 1L,
                              save_interim = FALSE), regexp = "Requires")
})
