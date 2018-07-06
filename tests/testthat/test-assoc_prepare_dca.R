context("test-assoc_prepare_dca.R")

collout <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig[1],
                          pattern = c("menggunakan"),
                          window = "b",
                          span = 2L)
assoc_tb <- assoc_prepare(collout)

test_that("assoc_prepare_dca produces error if the node word is not of the length 2", {
  expect_error(assoc_prepare_dca(assoc_tb),
               regexp = "^The number of the node words")
})

collout <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig[1],
                          pattern = c("menggunakan", "digunakan"),
                          window = "b",
                          span = 2L)
assoc_tb <- assoc_prepare(collout)

test_that("node input for assoc_prepare_dca should be of length 2", {
  expect_equal(length(unique(assoc_tb$node)), 2L)
})

dca_tb <- assoc_prepare_dca(assoc_tb)

test_that("assoc_prepare_dca has 6 columns", {
  expect_equal(dim(dca_tb)[2], 6L)
})
