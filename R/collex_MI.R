#' Mutual Information score
#'
#' @description The function to compute collocation association measure with Mutual Information.
#' @param df The output of \code{\link{assoc_prepare}}.
#' @param collstr_digit The numeric vector for floating digits of the collostruction strength. The default is \code{3}.
#'
#' @return A tibble consisting of the collocates (column \code{w}),
#'     co-occurrence frequencies with the node (column \code{a}),
#'     the expected co-occurrence frequencies with the node (column \code{a_exp}),
#'     the direction of the association (e.g., \emph{attraction} or \emph{repulsion}) (column \code{assoc}),
#'     the Mutual Information score (column \code{MI}),
#'     and two uni-directional association measures of \emph{Delta P}.
#' @export
#' @importFrom purrr pmap_dbl
#' @importFrom tidyr unnest
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
#' collex_MI(assoc_tb)
#'
#'
collex_MI <- function(df, collstr_digit = 3) {

  MI <- dplyr::quo(MI)
  a_exp <- dplyr::quo(a_exp)
  a <- dplyr::quo(a)
  b <- dplyr::quo(b)
  c <- dplyr::quo(c)
  d <- dplyr::quo(d)
  dP_collex_cue_cxn <- dplyr::quo(dP_collex_cue_cxn)
  dP_cxn_cue_collex <- dplyr::quo(dP_cxn_cue_collex)

  df_out <- tidyr::unnest(df, .data$data)
  df_out <- dplyr::mutate(df_out,
                          !!dplyr::quo_name(MI) := round(MI_compute(!!a, !!a_exp), digits = collstr_digit),
                          !!dplyr::quo_name(dP_collex_cue_cxn) := round(((!!a/(!!a + !!c)) - (!!b/(!!b + !!d))), digits = collstr_digit),
                          !!dplyr::quo_name(dP_cxn_cue_collex) := round(((!!a/(!!a + !!b)) - (!!c/(!!c + !!d))), digits = collstr_digit))

  df_out <- df_out[, -grep("^((b|c|d)(_exp)?|n_w_in_corp|corpus_size|n_pattern)$", colnames(df_out), perl = TRUE)]
  df_out <- dplyr::arrange(df_out, dplyr::desc(!!a))
  return(df_out)

}


#' Compute Mutual Information
#'
#' @description An internal function called by \code{\link{collex_MI}} to compute Mutual Information (MI) score
#'
#' @param a observed frequency co-occurrence of a collocate and the nodeword.
#' @param a_exp expected frequency co-occurrence of the collocate and the nodeword.
#'
#' @return A double-numeric value of MI score
#' @export
#'
#' @examples
#' # the numbers below are made-up examples
#' # for the collocate pattern "tindak pidana" 'legal act'
#' a <- 702
#' total_freq_w1 <- 1183
#' total_freq_w2 <- 1251
#' corpus_size <- 5763049
#' a_exp <- (total_freq_w1 * total_freq_w2)/corpus_size
#' (MI_score <- MI_compute(a, a_exp))
MI_compute <- function(a, a_exp) {

  return(log2((a/a_exp)))

}
