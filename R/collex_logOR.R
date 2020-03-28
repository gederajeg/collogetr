#' Log Odds Ratio
#'
#' @description The function to compute collocation association measure with the base 10 Log Odds Ratio (from Gries 2014).
#' @param df The output of \code{\link{assoc_prepare}}.
#' @param collstr_digit The numeric vector for floating digits of the collostruction strength. The default is \code{3}.
#'
#' @return A tibble consisting of the collocates (column \code{w}),
#'     co-occurrence frequencies with the node (column \code{a}),
#'     the expected co-occurrence frequencies with the node (column \code{a_exp}),
#'     the direction of the association (e.g., \emph{attraction} or \emph{repulsion}) (column \code{assoc}),
#'     the Log10 Odds Ratio score (column \code{logOR}),
#'     and two uni-directional association measures of \emph{Delta P}.
#' @export
#'
#' @examples
#' out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
#'                      pattern = "ke", # it is a preposition meaning 'to(wards)'
#'                      window = "r",
#'                      span = 2L,
#'                      save_interim = FALSE)
#' assoc_tb <- assoc_prepare(colloc_out = out,
#'             stopword_list = collogetr::stopwords[collogetr::stopwords != "ke"])
#'
#' collex_logOR(assoc_tb)
#'
#' @references
#' \itemize{
#'       \item Gries, S. T. (2014). Coll.analysis 3.5. A script for R to compute perform collostructional analyses. \url{http://www.linguistics.ucsb.edu/faculty/stgries/teaching/groningen/index.html}.
#' }
collex_logOR <- function(df, collstr_digit = 3) {


  logOR <- dplyr::quo(logOR)
  a <- dplyr::quo(a)
  b <- dplyr::quo(b)
  c <- dplyr::quo(c)
  d <- dplyr::quo(d)
  dP_collex_cue_cxn <- dplyr::quo(dP_collex_cue_cxn)
  dP_cxn_cue_collex <- dplyr::quo(dP_cxn_cue_collex)

  df_out <- tidyr::unnest(df, .data$data)
  df_out <- dplyr::mutate(df_out,
                          !!dplyr::quo_name(logOR) := round(logOR_compute(!!a, !!b, !!c, !!d), digits = collstr_digit),
                          !!dplyr::quo_name(dP_collex_cue_cxn) := round(((!!a/(!!a + !!c)) - (!!b/(!!b + !!d))), digits = collstr_digit),
                          !!dplyr::quo_name(dP_cxn_cue_collex) := round(((!!a/(!!a + !!b)) - (!!c/(!!c + !!d))), digits = collstr_digit))

  df_out <- df_out[, c(2, 1, 3, 10, 11, 12:14)]
  df_out <- dplyr::arrange(df_out, dplyr::desc(!!a))
  return(df_out)

}

#' Compute Log10 Odds Ratio
#'
#' @description internal function called by \code{\link{collex_logOR}} to compute Log10 Odds Ratio (Gries 2014).
#' @param a co-occurrence frequency between a given collocate and the nodeword.
#' @param b co-occurrence frequency between the collocate and other words.
#' @param c co-occurrence frequency between the nodeword and the other words.
#' @param d co-occurrence frequency between all other words.
#' @export
#'
#' @return A double-numeric value of Log10 Odds Ratio score
#'
#' @examples
#' # Data from Table 2 in Rajeg and Rajeg (2019:71)
#' a <- 9
#' b <- 1
#' c <- 408
#' d <- 305
#'
#' logOR_compute(a, b, c, d)
#'
#' # the above data produces positive log OR
#' # Observed freq. for cell "a" is larger than the Expected freq.
#'
#' # Data from Table 3 in Rajeg and Rajeg (2019:74)
#' a <- 1
#' b <- 6
#' c <- 416
#' d <- 300
#'
#' logOR_compute(a, b, c, d)
#'
#' # the above data produces negative log OR
#' # Expected freq. for cell "a" is larger than the Observed freq.
#'
#' @references
#'   \itemize{
#'       \item Rajeg, Gede Primahadi Wijaya & I Made Rajeg. 2019. Analisis Koleksem Khas dan potensinya untuk kajian kemiripan makna konstruksional dalam Bahasa Indonesia. In I Nengah Sudipa (ed.), \emph{ETIKA BAHASA Buku persembahan menapaki usia pensiun: I Ketut Tika}, vol. 1, 65–83. Denpasar, Bali, Indonesia: Swasta Nulus. \url{https://doi.org10.26180/5bf4e49ea1582}. \url{https://osf.io/preprints/inarxiv/uwzts/} (30 January, 2019).
#'       \item Gries, S. T. (2014). Coll.analysis 3.5. A script for R to compute perform collostructional analyses. \url{http://www.linguistics.ucsb.edu/faculty/stgries/teaching/groningen/index.html}.
#' }
logOR_compute <- function(a, b, c, d) {

  return(log10(((a + 0.5)/(b + 0.5))/((c + 0.5)/(d + 0.5))))

}
