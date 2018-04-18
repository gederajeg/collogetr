#' Fisher's Exact Test
#'
#' @description Perform one-tailed Fisher's Exact test for the collostruction/collocation strength.
#'     The \emph{p}-value is log-transformed to the base of ten as in the \emph{Collostructional Analysis}.
#' @param df The output of \code{\link{assoc_prepare}}.
#' @param mpfr_precision Integer indicating the maximal precision to be used in \bold{bits}. This is passed to the \code{precBits} argument of \code{\link[Rmpfr]{mpfr}}.
#' @param collstr_digit The floating digits of the collostruction strength. The default is \code{3}.
#'
#' @return A tibble consisting of the collocates (column \code{w}),
#'     co-occurrence frequencies with the node (column \code{a}),
#'     the expected co-occurrence frequencies with the node (column \code{a_exp}),
#'     and the collostruction strength (column \code{collstr}).
#' @export
#' @importFrom Rmpfr asNumeric
#' @importFrom Rmpfr mpfr
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
#' @examples
#' out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig[1:4],
#'                       pattern = "\\bke\\b",
#'                       window = "r",
#'                       span = 1,
#'                       save_interim = FALSE)
#' assoc_tb <- assoc_prepare(colloc_out = out, stopword_list = stopwords, mpfr_precision = 120)
#' am_fye <- fye(df = assoc_tb, mpfr_precision = 120, collstr_digit = 3)
#'
fye <- function(df, mpfr_precision = NULL, collstr_digit = 3) {
  df <- dplyr::mutate(df, collstr = purrr::map_dbl(data, fye_compute, mpfr_precision, collstr_digit)) # perform FYE with purrr-style using fye_compute
  df_out <- dplyr::arrange(df, dplyr::desc(collstr))
  df_out <- tidyr::unnest(df_out)
  df_out <- dplyr::select(df_out, .data$w, .data$a, .data$a_exp, .data$assoc, .data$collstr)
  return(df_out)
}
