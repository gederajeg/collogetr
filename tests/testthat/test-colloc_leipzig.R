context("test-colloc_leipzig.R")

out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig[1:4],
                      pattern = "\\bke\\b",
                      window = "r",
                      span = 1,
                      save_interim = FALSE)

testthat::test_that("output of colloc_leipzig is a list of six elements", {
  expect_output(str(out), "List of 6")
})


testthat::test_that("non-matching pattern of colloc_leipzig produces message", {
  expect_message(colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig[1:2],
                                leipzig_path = NULL,
                                pattern = "\\bmemilikihgjdgjdsnvjsvnsj\\b",
                                window = "r",
                                span = 1,
                                save_interim = FALSE), regexp = "^No match found.+?inputs.$", perl = TRUE)
})
