#' Fisher's Exact Test
#'
#' @description Perform one-tailed Fisher's Exact test for the collostruction/collocation strength.
#'     It internally calls the utility function \code{\link{fye_compute}} and performs a row-wise computation using \code{\link[purrr]{map_dbl}}.
#'     The \emph{p}-value is log-transformed to the base of ten as in the \emph{Collostructional Analysis}.
#' @param df The output of \code{\link{assoc_prepare}}.
#' @param collstr_digit The numeric vector for floating digits of the collostruction strength. The default is \code{3}.
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
#' @importFrom stats dhyper
#' @importFrom purrr is_null
#' @importFrom purrr map_dbl
#' @importFrom tidyr unnest
#' @importFrom utils data
#' @examples
#' out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
#'                       pattern = "mengatakan",
#'                       window = "r",
#'                       span = 3L,
#'                       save_interim = FALSE)
#' assoc_tb <- assoc_prepare(colloc_out = out, stopword_list = stopwords)
#' am_fye <- collex_fye(df = assoc_tb, collstr_digit = 3)
#'
collex_fye <- function(df, collstr_digit = 3) {
  collstr <- dplyr::quo(collstr)
  p_fye <- dplyr::quo(p_fye)
  a <- dplyr::quo(a)
  a_exp <- dplyr::quo(a_exp)
  b <- dplyr::quo(b)
  c <- dplyr::quo(c)
  d <- dplyr::quo(d)
  dP_collex_cue_cxn <- dplyr::quo(dP_collex_cue_cxn)
  dP_cxn_cue_collex <- dplyr::quo(dP_cxn_cue_collex)
  df_out <- tidyr::unnest(df, .data$data)
  df_out <- dplyr::mutate(df_out,
                          !!dplyr::quo_name(p_fye) := fye_compute(!!a, !!a_exp, !!b, !!c, !!d),
                          !!dplyr::quo_name(dP_collex_cue_cxn) := round(((!!a/(!!a + !!c)) - (!!b/(!!b + !!d))), digits = collstr_digit),
                          !!dplyr::quo_name(dP_cxn_cue_collex) := round(((!!a/(!!a + !!b)) - (!!c/(!!c + !!d))), digits = collstr_digit))
  df_out <- dplyr::mutate(df_out,
                          !!dplyr::quo_name(collstr) := dplyr::if_else(!!a > !!a_exp,
                                                                       round(-log10(!!p_fye), collstr_digit),
                                                                       round(log10(!!p_fye), collstr_digit)))
  df_out <- dplyr::arrange(df_out, dplyr::desc(collstr))
  df_out <- df_out[, -grep("^((b|c|d)(_exp)?|n_w_in_corp|corpus_size|n_pattern)$", colnames(df_out), perl = TRUE)]
  # df_out <- dplyr::select(df_out, -.data$b, -.data$d, -.data$c, -.data$n_pattern, -.data$n_w_in_corp, -.data$corpus_size)
  # df_out <- dplyr::select(df_out, .data$w, .data$node, .data$a, .data$a_exp, .data$assoc, .data$p_fye, .data$collstr, dplyr::everything())
  return(df_out)
}

#' Fisher's Exact Test
#'
#' @description Perform one-tailed Fisher's Exact test for the collostruction/collocation strength.
#'     The \emph{p}-value is log-transformed to the base of ten as in the \emph{Collostructional Analysis}.
#'     This is an internal function called via \code{\link{collex_fye}}.
#' @param a observed co-occurrence frequency of a given collocate and the nodeword.
#' @param a_exp expected co-occurrence frequency of the collocate and the nodeword.
#' @param b co-occurrence frequency of the collocate with other words in the corpus.
#' @param c co-occurrence frequency of the nodeword with other words in the corpus.
#' @param d co-occurrence frequency of other words in the corpus.
#'
#' @return Numeric vectors of p-values
#'
#'
fye_compute <- function(a, a_exp, b, c, d) {

  if (a > a_exp) {

    pfye <- sum(dhyper(a:(a+c), (a+c), (sum(c(a, b, c, d))-(a+c)), (a+b)))

  } else {

    pfye <- sum(dhyper(0:a, (a+c), (sum(c(a, b, c, d))-(a+c)), (a+b)))

  }

  return(pfye)
}

#' Get the expected frequency
#'
#' @description Generate expected frequency for cell \emph{a} based on the output of Chi-square Test.
#'     It is called internally via \code{\link{assoc_prepare}}.
#' @param df Nested data frame
#' @param collstr_digit The numeric vector for the floating digits to keep. It is passed from \code{\link{assoc_prepare}}.
#' @importFrom stats chisq.test
#'
#' @return A double vector
exp_freq <- function(df, collstr_digit) {
  mtx <- cbind(c(df$a, df$c), c(df$b, df$d))
  exp <- round(suppressWarnings(stats::chisq.test(mtx, correct = TRUE)$expected[1,1]), collstr_digit)
  return(exp)
}
