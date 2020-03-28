context("collex_logOR")

out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
                      pattern = "mengatakan", # it is a preposition meaning 'to(wards)'
                      window = "b",
                      span = 2L,
                      save_interim = FALSE)
assoc_tb <- assoc_prepare(colloc_out = out,
                          stopword_list = collogetr::stopwords)
log_odds <- collex_logOR(assoc_tb)

testthat::test_that("Log10 Odds Ratio values are double", {
  testthat::expect_type(log_odds$logOR, "double")
})

assoc_tb <- assoc_prepare(colloc_out = out,
                          window_span = "l1",
                          per_corpus = TRUE,
                          stopword_list = NULL)
log_odds <- collex_logOR(assoc_tb)

testthat::test_that("Log10 Odds Ratio values are double", {
  testthat::expect_type(log_odds$logOR, "double")
})
