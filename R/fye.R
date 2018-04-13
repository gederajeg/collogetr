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
#' @examples
#' out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig[1:4],
#'                       pattern = "\\bke\\b",
#'                       window = "r",
#'                       span = 1,
#'                       save_interim = FALSE)
#' assoc_tb <- assoc_prepare(colloc_out = out, stopword_list = stopwords)
#' am_fye <- fye(df = assoc_tb, mpfr_precision = 120, collstr_digit = 3)
#' am_fye[order(-am_fye$collstr),]
#'
fye <- function(df, mpfr_precision = 120, collstr_digit = 3) {

  # generate results vector
  results <- vector(mode = "numeric", length = length(df$a))

  # generate progress bar estimates
  p <- dplyr::progress_estimated(length(df$a))

  # generate the expected co-occurrence frequencies
  margin_calculation <- Rmpfr::asNumeric(Rmpfr::mpfr((df$n_w_in_corp * df$n_pattern), mpfr_precision))
  corpus_size <- df$corpus_size
  a_exp <- Rmpfr::mpfr((margin_calculation/corpus_size), mpfr_precision)
  a_exp <- Rmpfr::asNumeric(a_exp)

  # calucate fye
  for (i in seq_along(df$a)) {

    # print progress bar
    p$pause(0.1)$tick()$print()

    # create a 2-by-2 matrix for FYE input
    cross_table <- rbind(c(df$a[i], df$b[i]), c(df$c[i], df$d[i]))

    if (df$a[i] > a_exp[i]) {
      output <- Rmpfr::asNumeric(Rmpfr::mpfr(stats::fisher.test(cross_table, alternative = "greater")$p.value, mpfr_precision))
      output <- round(-log10(output), digits = collstr_digit)
    } else {
      output <- Rmpfr::asNumeric(Rmpfr::mpfr(stats::fisher.test(cross_table, alternative = "less")$p.value, mpfr_precision))
      output <- round(-log10(output), digits = collstr_digit)
    }

    results[i] <- output
  }
  df$collstr <- results
  df$a_exp <- a_exp
  assoc <- dplyr::quo(assoc)
  df <- dplyr::mutate(df,
                      !!dplyr::quo_name(assoc) := "neutral",
                      !!dplyr::quo_name(assoc) := replace(.data$assoc, .data$a > .data$a_exp, "attraction"),
                      !!dplyr::quo_name(assoc) := replace(.data$assoc, .data$a < .data$a_exp, "repulsion"))
  df_out <- dplyr::select(df, .data$w, .data$a, .data$a_exp, .data$assoc, .data$collstr)
  df_out <- dplyr::arrange(df_out, dplyr::desc(.data$collstr))
  return(df_out)
}
