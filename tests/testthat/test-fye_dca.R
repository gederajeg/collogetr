context("test-fye_dca.R")

collout <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
                          pattern = c("akan", "mau"),
                          window = "r",
                          span = 1L)
assoc_tb <- assoc_prepare(collout,
                          window_span = NULL,
                          stopword_list = stopwords[!stopwords %in% c("akan", "mau")])
dca_tb <- assoc_prepare_dca(assoc_tb)
dca_res <- collex_fye_dca(dca_tb)

test_that("the output of collex_fye_dca is of six columns", {
  expect_equal(dim(dca_res)[2], 6L)
})

test_that("the identicality of the column names of the dca output", {
  expect_equivalent(colnames(dca_res)[-c(2,3)], c("w", "p_fye", "collstr", "dist_for"))
})

test_that("the structure of the output is okay", {
  expect_output(str(dca_res), "$ dist_for", fixed = TRUE)
  expect_output(str(dca_res), "$ collstr", fixed = TRUE)
  expect_output(str(dca_res), "$ p_fye", fixed = TRUE)
  expect_output(str(dca_res), "tbl_df", fixed = TRUE)
})

test_that("the output of dca_top_collex() for cxn 'a' return the same value between the name for column 2 and value for dist_for", {
  expect_equivalent(colnames(dca_top_collex(dca_res, "a"))[2], unique(dca_top_collex(dca_res, "a")$dist_for))
})

test_that("the output of dca_top_collex() for cxn 'b' return the same value between the name for column 3 and value for dist_for", {
  expect_equivalent(colnames(dca_top_collex(dca_res, "b"))[3], unique(dca_top_collex(dca_res, "b")$dist_for))
})

test_that("the structure of the output is okay", {
  expect_output(str(dca_top_collex(dca_res, "a")), "$ dist_for", fixed = TRUE)
  expect_output(str(dca_top_collex(dca_res, "a")), "$ collstr", fixed = TRUE)
  expect_output(str(dca_top_collex(dca_res, "a")), "$ p_fye", fixed = TRUE)
  expect_output(str(dca_top_collex(dca_res, "a")), "data.frame", fixed = TRUE)
  expect_output(str(dca_top_collex(dca_res, "a", top_n = NULL)), "tbl_df", fixed = TRUE)
})
