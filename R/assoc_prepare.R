
#' Generate frequency table for association measure
#'
#' @description The function to produce frequency table required as input for association measures for collocations
#' @param colloc_out The output list of \code{\link{colloc_leipzig}}.
#' @param stopword_list Character vectors containing list of stopwords to be removed from the collocation measures.
#' @param mpfr_precision Integer indicating the maximal precision to be used in \bold{bits}. This is passed to the \code{precBits} argument of \code{\link[Rmpfr]{mpfr}}.
#'     It is included to handle error produced when many floating points cannot be handled by the CPU.
#'     The default is \code{NULL}.
#' @export
#' @importFrom purrr is_null
#' @importFrom dplyr filter
#' @importFrom dplyr tally
#' @importFrom dplyr inner_join
#' @importFrom dplyr pull
#' @importFrom dplyr quo_name
#' @importFrom dplyr quo
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom stringr str_detect
#' @importFrom tidyr nest
#'
#' @return A tbl_df of two columns. One of them is nested columns with input-data for row-wise Fisher-Exact Test with \code{\link{collex_fye}}.
#' @examples
#' \dontrun{
#'  assoc_tb <- assoc_prepare(colloc_leipzig_output, stopword_list = NULL)
#' }
#'
assoc_prepare <- function(colloc_out = NULL, stopword_list = NULL, mpfr_precision = NULL) {

  # check if stopwords removed from the calculation of collocation strength
  if(!purrr::is_null(stopword_list)) {
    all_colloc_freq <- dplyr::filter(colloc_out$collocs_freq, !.data$w %in% stopword_list)
    all_words_freq <- dplyr::filter(colloc_out$words_freq, !.data$w %in% stopword_list)
    all_corpus_size <- unname(unlist(dplyr::tally(all_words_freq, .data$n_w_in_corp)))
    message("You chose to remove stopwords!")
  } else {
    all_colloc_freq <- colloc_out$collocs_freq
    all_words_freq <- colloc_out$words_freq
    all_corpus_size <- colloc_out$all_corpus_size
    message("Stopwords are retained!")
  }

  # merge the two tables of collocates frequency with the node; their frequency in the corpus; and the total size of the corpus
  assoc_tb <- dplyr::inner_join(all_colloc_freq, all_words_freq, by = "w")

  # add the total size of the corpus
  assoc_tb$corpus_size <- all_corpus_size

  # get the frequency of the node pattern in the corpus
  assoc_tb$n_pattern <- dplyr::pull(dplyr::filter(colloc_out$words_freq,
                                                  stringr::str_detect(.data$w, colloc_out$node_regex_exact)),
                                    .data$n_w_in_corp)

  # prepare quoted variable for mutate variables
  b <- dplyr::quo(b)
  c <- dplyr::quo(c)
  d <- dplyr::quo(d)
  assoc <- dplyr::quo(assoc)
  w <- dplyr::quo(w)

  # get the frequency for the contigency table inputs
  assoc_tb <- dplyr::mutate(assoc_tb,
                            !!dplyr::quo_name(b) := .data$n_w_in_corp - .data$a,
                            !!dplyr::quo_name(c) := .data$n_pattern - .data$a,
                            !!dplyr::quo_name(d) := .data$corpus_size - (.data$a + .data$b + .data$c))


  # compute the expected co-occurrence frequency
  if (!purrr::is_null(mpfr_precision)) {
    margin_product <- Rmpfr::asNumeric(Rmpfr::mpfr((assoc_tb$n_w_in_corp * assoc_tb$n_pattern), mpfr_precision))
    a_exp <- Rmpfr::asNumeric(Rmpfr::mpfr((margin_product/assoc_tb$corpus_size), mpfr_precision))
  } else {
    margin_product <- assoc_tb$n_w_in_corp * assoc_tb$n_pattern
    a_exp <- margin_product/assoc_tb$corpus_size
  }

  # add the expected frequency into the tibble
  assoc_tb$a_exp <- a_exp

  # add association direction
  assoc_tb <- dplyr::mutate(assoc_tb,
                            !!dplyr::quo_name(assoc) := "neutral",
                            !!dplyr::quo_name(assoc) := replace(.data$assoc, .data$a > .data$a_exp, "attraction"),
                            !!dplyr::quo_name(assoc) := replace(.data$assoc, .data$a < .data$a_exp, "repulsion"))

  assoc_tb <- tidyr::nest(dplyr::group_by(assoc_tb, !!w)) # nest the data columns required for row-wise FYE with purrr map
  return(assoc_tb)
}
