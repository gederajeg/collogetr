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
  expect_warning(colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
                                leipzig_path = NULL,
                                pattern = "memilikihgjdgjdsnvjsvnsj",
                                window = "r",
                                span = 1L,
                                save_interim = FALSE), regexp = "(No match is detected|SORRY)", perl = TRUE, all = TRUE)
})

testthat::test_that("NULL corpus inputs and NULL pattern input produce error", {
  expect_error(colloc_leipzig(leipzig_corpus_list = NULL,
                              leipzig_path = NULL,
                              pattern = NULL,
                              window = "r",
                              span = 1L,
                              save_interim = FALSE), regexp = "Requires")
})

testthat::test_that("message is out for the input file type", {
  expect_message(colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig[2:3],
                              leipzig_path = NULL,
                              pattern = "memberikan",
                              window = "r",
                              span = 1L,
                              save_interim = FALSE), regexp = "input", all = FALSE, perl = TRUE)
})

testthat::test_that("message is out for the save_interim", {
  expect_message(colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig[2:3],
                                leipzig_path = NULL,
                                pattern = "memberikan",
                                window = "r",
                                span = 1L,
                                save_interim = FALSE), regexp = "SAVE INTERIM", all = FALSE, perl = TRUE)
  expect_message(colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig[2:3],
                                leipzig_path = NULL,
                                pattern = "memberikan",
                                window = "r",
                                span = 1L,
                                save_interim = TRUE), regexp = "Generating output files", all = FALSE, perl = TRUE)
})

