#' Fisher's Exact Test
#'
#' @description Perform one-tailed Fisher's Exact test for the collostruction/collocation strength.
#'     It internally calls the utility function \code{\link{fye_compute}} and performs a row-wise computation using \code{\link[purrr]{map_dbl}}.
#'     The \emph{p}-value is log-transformed to the base of ten as in the \emph{Collostructional Analysis}.
#' @param df The output of \code{\link{assoc_prepare}}.
#' @param collstr_digit The floating digits of the collostruction strength. The default is \code{3}.
#'
#' @return A tibble consisting of the collocates (column \code{w}),
#'     co-occurrence frequencies with the node (column \code{a}),
#'     the expected co-occurrence frequencies with the node (column \code{a_exp}),
#'     the direction of the association (e.g., \emph{attraction} or \emph{repulsion}) (column \code{assoc}),
#'     the collostruction strength (column \code{collstr}),
#'     and two uni-directional association measures of \emph{Delta P}.
#' @export
#' @importFrom dplyr progress_estimated
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @importFrom dplyr quo_name
#' @importFrom dplyr quo
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr desc
#' @importFrom rlang :=
#' @importFrom stats fisher.test
#' @importFrom purrr is_null
#' @importFrom purrr map_dbl
#' @importFrom tidyr unnest
#' @importFrom utils data
#' @examples
#' out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
#'                       pattern = "\\bakan\\b",
#'                       window = "r",
#'                       span = 3,
#'                       save_interim = FALSE)
#' assoc_tb <- assoc_prepare(colloc_out = out, stopword_list = stopwords)
#' am_fye <- collex_fye(df = assoc_tb, collstr_digit = 3)
#'
collex_fye <- function(df, collstr_digit = 3) {
  collstr <- dplyr::quo(collstr)
  dP_collex_cue_cxn <- dplyr::quo(dP_collex_cue_cxn)
  dP_cxn_cue_collex <- dplyr::quo(dP_cxn_cue_collex)
  df <- dplyr::mutate(df, !!dplyr::quo_name(collstr) := purrr::map_dbl(data, fye_compute, collstr_digit = collstr_digit),
                      !!dplyr::quo_name(dP_collex_cue_cxn) := purrr::map_dbl(data, dP_cue_cxn, collstr_digit = collstr_digit),
                      !!dplyr::quo_name(dP_cxn_cue_collex) := purrr::map_dbl(data, dP_cue_collex, collstr_digit = collstr_digit))
  df_out <- dplyr::arrange(df, dplyr::desc(collstr))
  df_out <- tidyr::unnest(df_out)
  df_out <- dplyr::select(df_out, .data$w, .data$a, .data$a_exp, .data$assoc, .data$collstr, .data$dP_collex_cue_cxn, .data$dP_cxn_cue_collex)
  return(df_out)
}
