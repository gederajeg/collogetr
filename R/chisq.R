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
#'     and the chi-square-based collostruction strength (column \code{chisq}).
#' @export
#' @examples
#' out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
#'                       pattern = "\\bakan\\b",
#'                       window = "r",
#'                       span = 3,
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
  df <- dplyr::mutate(df, !!dplyr::quo_name(chisq) := purrr::map_dbl(data, chisq_compute, collstr_digit = collstr_digit))
  df_out <- dplyr::arrange(df, dplyr::desc(chisq))
  df_out <- tidyr::unnest(df_out)
  df_out <- dplyr::select(df_out, .data$w, .data$a, .data$a_exp, .data$assoc, .data$chisq)
  return(df_out)
}
