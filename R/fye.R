#' Fisher's Exact Test
#'
#' @description Perform one-tailed Fisher's Exact test for the collostruction/collocation strength.
#'     It internally calls the utility function \code{\link{fye_compute}} and performs a row-wise computation using \code{\link[purrr]{map_dbl}}.
#'     The \emph{p}-value is log-transformed to the base of ten as in the \emph{Collostructional Analysis}.
#' @param df The output of \code{\link{assoc_prepare}}.
#' @param collstr_digit The integer for floating digits of the collostruction strength. The default is \code{3L}.
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
#' am_fye <- collex_fye(df = assoc_tb, collstr_digit = 3L)
#'
collex_fye <- function(df, collstr_digit = 3L) {
  collstr <- dplyr::quo(collstr)
  p_fye <- dplyr::quo(p_fye)
  dP_collex_cue_cxn <- dplyr::quo(dP_collex_cue_cxn)
  dP_cxn_cue_collex <- dplyr::quo(dP_cxn_cue_collex)
  df <- dplyr::mutate(df,
                      !!dplyr::quo_name(p_fye) := purrr::map_dbl(data, fye_compute),
                      !!dplyr::quo_name(dP_collex_cue_cxn) := purrr::map_dbl(data, dP_cue_cxn, collstr_digit = collstr_digit),
                      !!dplyr::quo_name(dP_cxn_cue_collex) := purrr::map_dbl(data, dP_cue_collex, collstr_digit = collstr_digit))
  df_out <- tidyr::unnest(df)
  df_out <- dplyr::mutate(df_out,
                          !!dplyr::quo_name(collstr) := dplyr::if_else(.data$a > .data$a_exp,
                                                                       round(-log10(.data$p_fye), collstr_digit),
                                                                       round(log10(.data$p_fye), collstr_digit)))
  df_out <- dplyr::arrange(df_out, dplyr::desc(collstr))
  df_out <- dplyr::select(df_out, -.data$b, -.data$d, -.data$c, -.data$n_pattern, -.data$n_w_in_corp, -.data$corpus_size)
  df_out <- dplyr::select(df_out, .data$w, .data$node, .data$a, .data$a_exp, .data$assoc, .data$p_fye, .data$collstr, dplyr::everything())
  return(df_out)
}

#' Fisher's Exact Test
#'
#' @description Perform one-tailed Fisher's Exact test for the collostruction/collocation strength.
#'     The \emph{p}-value is log-transformed to the base of ten as in the \emph{Collostructional Analysis}.
#'     This is an internal function called via \code{\link{collex_fye}}.
#' @param df The output of \code{\link{assoc_prepare}}.
#'
#' @return Numeric vectors of p-values
#'
#'
fye_compute <- function(df) {

  # get into crosstabulation format
  crosstab <- rbind(c(df$a, df$b), c(df$c, df$d))

  # FYE computation
  fye_pval <- dplyr::if_else(df$a > df$a_exp,
                             stats::fisher.test(crosstab, alternative = "greater")$p.value,
                             stats::fisher.test(crosstab, alternative = "less")$p.value)
  return(fye_pval)
}

#' Get the expected frequency
#'
#' @description Generate expected frequency for cell \emph{a} based on the output of Chi-square Test.
#'     It is called internally via \code{\link{assoc_prepare}}.
#' @param df Nested data frame
#' @param collstr_digit Floating points to keep
#' @importFrom stats chisq.test
#'
#' @return A double vector
exp_freq <- function(df, collstr_digit) {
  mtx <- cbind(c(df$a, df$c), c(df$b, df$d))
  exp <- round(suppressWarnings(stats::chisq.test(mtx, correct = TRUE)$expected[1,1]), collstr_digit)
  return(exp)
}
