#' Chi-square test for association measure
#'
#' @description Compute collostructional strength based on the chi-square contribution.
#'     It internally calls the utility function \code{\link{chisq_compute}} and performs a row-wise computation using \code{\link[purrr]{map_dbl}}.
#' @param df The output of \code{\link{assoc_prepare}}.
#' @param collstr_digit The floating digits of the collostruction strength. The default is \code{3}.
#'
#' @return A tibble consisting of the collocates (column \code{w}),
#'     co-occurrence frequencies with the node (column \code{a}),
#'     the expected co-occurrence frequencies with the node (column \code{a_exp}),
#'     the direction of the association (e.g., \emph{attraction} or \emph{repulsion}) (column \code{assoc}),
#'     the chi-square-based collostruction strength (column \code{chisq}), and two uni-directional association measures of \emph{Delta P}.
#' @export
#' @examples
#' out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
#'                       pattern = "mengatakan",
#'                       window = "r",
#'                       span = 3L,
#'                       save_interim = FALSE)
#' assoc_tb <- assoc_prepare(colloc_out = out, stopword_list = stopwords)
#' am_chisq <- collex_chisq(df = assoc_tb, collstr_digit = 3)
#' @importFrom dplyr quo
#' @importFrom dplyr quo_name
#' @importFrom dplyr select
#' @importFrom dplyr desc
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom purrr map_dbl
#' @importFrom rlang !!
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#'
collex_chisq <- function(df, collstr_digit = 3) {
  chisq <- dplyr::quo(chisq)
  dP_collex_cue_cxn <- dplyr::quo(dP_collex_cue_cxn)
  dP_cxn_cue_collex <- dplyr::quo(dP_cxn_cue_collex)
  df <- dplyr::mutate(df,
                      !!dplyr::quo_name(chisq) := purrr::map_dbl(data, chisq_compute, collstr_digit = collstr_digit),
                      !!dplyr::quo_name(dP_collex_cue_cxn) := purrr::map_dbl(data, dP_cue_cxn, collstr_digit = collstr_digit),
                      !!dplyr::quo_name(dP_cxn_cue_collex) := purrr::map_dbl(data, dP_cue_collex, collstr_digit = collstr_digit))
  df_out <- dplyr::arrange(df, dplyr::desc(chisq))
  df_out <- tidyr::unnest(df_out, .data$data)
  df_out <- df_out[, -grep("^((b|c|d)(_exp)?|n_w_in_corp|corpus_size|n_pattern)$", colnames(df_out), perl = TRUE)]
  # df_out <- dplyr::select(df_out, .data$w, .data$a, .data$a_exp, .data$assoc, .data$chisq, .data$dP_collex_cue_cxn, .data$dP_cxn_cue_collex)
  return(df_out)
}

#' Chi-square contribution
#'
#' @description Compute the chi-square contribution as the association measure of the co-occurrence cell (i.e., cell \code{a}).
#'     This is an internal function called via \code{\link{collex_chisq}}.
#' @param df The output of \code{\link{assoc_prepare}}.
#' @param collstr_digit The floating digits of the collostruction strength. It is passed on from \code{\link{collex_chisq}} and the default is \code{3}.
#'
#' @return A double vector of collostruction strength based on chi-square contribution
chisq_compute <- function(df, collstr_digit = NULL) {

  # compute chi-square contribution of cell `a` that contains co-occurrence frequency between the node and the collocates
  chisq_a <- ((df$a - df$a_exp) ^ 2)/df$a_exp
  if (purrr::is_null(collstr_digit)) {
    return(chisq_a)
  } else {
    return(round(chisq_a, collstr_digit))
  }
}
