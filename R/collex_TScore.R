#' T-Score association measure
#'
#' @description The function to compute collocation association measure with T-Score.
#' @param df The output of \code{\link{assoc_prepare}}.
#'
#' @return A tibble consisting of the collocates (column \code{w}),
#'     co-occurrence frequencies with the node (column \code{a}),
#'     the expected co-occurrence frequencies with the node (column \code{a_exp}),
#'     the direction of the association (e.g., \emph{attraction} or \emph{repulsion}) (column \code{assoc}),
#'     the T-Score (column \code{tscore}),
#'     and two uni-directional association measures of \emph{Delta P}.
#' @export
#'
#' @examples
#' out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
#'     pattern = "ke", # it is a preposition meaning 'to(wards)'
#'     window = "r",
#'     span = 2L,
#'     save_interim = FALSE)
#' assoc_tb <- assoc_prepare(colloc_out = out,
#'      stopword_list = collogetr::stopwords[collogetr::stopwords != "ke"])
#'
#' collex_TScore(assoc_tb)
#'
collex_TScore <- function(df) {

  tscore <- dplyr::quo(tscore)
  a <- dplyr::quo(a)
  b <- dplyr::quo(b)
  c <- dplyr::quo(c)
  d <- dplyr::quo(d)
  n_w_in_corp <- dplyr::quo(n_w_in_corp)
  n_pattern <- dplyr::quo(n_pattern)
  corpus_size <- dplyr::quo(corpus_size)
  dP_collex_cue_cxn <- dplyr::quo(dP_collex_cue_cxn)
  dP_cxn_cue_collex <- dplyr::quo(dP_cxn_cue_collex)

  df_out <- tidyr::unnest(df, .data$data)
  df_out <- dplyr::mutate(df_out,
                          !!dplyr::quo_name(tscore) := purrr::pmap_dbl(list(!!a,
                                                                        !!n_w_in_corp,
                                                                        !!n_pattern,
                                                                        !!corpus_size),
                                                                   Tscore_compute),
                          !!dplyr::quo_name(dP_collex_cue_cxn) := purrr::pmap_dbl(list(!!a,
                                                                                       !!b,
                                                                                       !!c,
                                                                                       !!d),
                                                                                  function(a, b, c, d) (a/(a + c)) - (b/(b + d))),
                          !!dplyr::quo_name(dP_cxn_cue_collex) := purrr::pmap_dbl(list(!!a,
                                                                                       !!b,
                                                                                       !!c,
                                                                                       !!d),
                                                                                  function(a, b, c, d) (a/(a + b)) - (c/(c + d)))
  )

  df_out <- df_out[, c(2, 1, 3, 10, 11, 12:14)]
  df_out <- dplyr::arrange(df_out, dplyr::desc(!!a))
  return(df_out)

}


#' Compute T-Score
#'
#' @description An internal function called by \code{\link{collex_TScore}} to compute T-Score values
#'
#' @param a frequency co-occurrence of a collocate and the nodeword.
#' @param b overall frequency of the collocate in the corpus
#' @param c overall frequency of the nodeword in the corpus
#' @param d total size of the corpus
#'
#' @return A double-numeric value of T-Score
#' @export
#'
#' @examples
#' # the numbers below are made-up examples
#' # for the collocate pattern "tindak pidana" 'legal act'
#' a <- 702
#' b <- 1183
#' c <- 1251
#' d <- 5763049
#' (tscore <- Tscore_compute(a, b, c, d))
Tscore_compute <- function(a, b, c, d) {

  return((a - ((b * c)/d))/sqrt(a))

}
