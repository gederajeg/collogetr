#' Generate frequency table for association measure
#'
#' @description The function to produce frequency table required as input for association measures for collocations
#' @param colloc_out The output list of \code{\link{colloc_leipzig}}.
#' @param window_span Specify the window and span combination of the collocates to focus on for the measure (e.g., \code{"r1"} for 1 word to the right of the node; or a set of values as in \code{c("r1", "r2")}). The default is \code{NULL}.
#' @param per_corpus Logical; whether to process the collocates per corpus file (\code{TRUE}) or aggregate the data across the corpus files (\code{FALSE}).
#' @param stopword_list Character vectors containing list of stopwords to be removed from the collocation measures.
#' @param float_digits The numeric vector for floating digits of the expected frequency values. The default is \code{3}.
#' @export
#' @importFrom purrr is_null
#' @importFrom dplyr filter
#' @importFrom dplyr tally
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr count
#' @importFrom dplyr arrange
#' @importFrom dplyr pull
#' @importFrom dplyr quo_name
#' @importFrom dplyr quo
#' @importFrom readr read_tsv
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom stringr str_detect
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#'
#' @return A tbl_df of two columns. One of them is nested columns with input-data for row-wise association measure calculation (e.g., the Fisher-Exact Test with \code{\link{collex_fye}}).
#' @examples
#' \dontrun{
#'  # (1) if the colloc_leipzig() output is stored as list on console
#'  assoc_tb <- assoc_prepare(colloc_out = colloc_leipzig_output,
#'                            window_span = "r1",
#'                            per_corpus = FALSE,
#'                            stopword_list = NULL,
#'                            float_digits = 3)
#'
#' # (2) if the output of colloc_leipzig() is saved into disk
#'       supply the vector of output file names
#' ## (2.1) Run colloc_leipzig with "save_interim = TRUE"
#' outfiles <- colloc_leipzig(leipzig_path = my_leipzig_path,
#'                            pattern = my_pattern,
#'                            window = "r",
#'                            span = 3,
#'                            save_interim = TRUE # save interim results to disk
#'                            freqlist_output_file = "~/Desktop/out_1_freqlist.txt",
#'                            colloc_output_file = "~/Desktop/out_2_collocates.txt",
#'                            corpussize_output_file = "~/Desktop/out_3_corpus_size.txt",
#'                            search_pattern_output_file = "~/Desktop/out_4_search_pattern.txt"
#'                            )
#' ## (2.2) supply colloc_out with "outfiles"
#' assoc_tb <- assoc_prepare(colloc_out = outfiles,
#'                           window_span = "r1",
#'                           per_corpus = FALSE,
#'                           stopword_list = stopwords,
#'                           float_digits = 3)
#' }
#'
assoc_prepare <- function(colloc_out = NULL,
                          window_span = NULL,
                          per_corpus = FALSE,
                          stopword_list = NULL,
                          float_digits = 3) {

  # assoc_prepare() function-body begins here

  if (is.list(colloc_out)) {
    message("Your colloc_leipzig output is stored as list!\n")
    colloc_df <- colloc_out$colloc_df
    freqlist_df <- colloc_out$freqlist_df
    corpussize_df <- colloc_out$corpussize_df
    search_pattern <- colloc_out$pattern
  } else {
    message("Your colloc_leipzig output comes from saved files on computer!\n")
    freqlist_df <- readr::read_tsv(file = colloc_out[1])
    colloc_df <- readr::read_tsv(file = colloc_out[2])
    corpussize_df <- readr::read_tsv(file = colloc_out[3])
    search_pattern <- readr::read_lines(file = colloc_out[4])
  }

  # prepare quoted variable
  n_w_in_corp <- dplyr::quo(n_w_in_corp)
  a <- dplyr::quo(a)
  corpus_names <- dplyr::quo(corpus_names)
  corpus_size <- dplyr::quo(corpus_size)
  node <- dplyr::quo(node)
  n_pattern <- dplyr::quo(n_pattern)

  # if per_corpus == FALSE, user wants to take aggregated collocational and frequency list data from all corpora
  if (per_corpus == FALSE) {
    message("You chose to combine the collocational and frequency list data from ALL CORPORA!")
    colloc_df <- dplyr::group_by(colloc_df, .data$node)
    message("Tallying frequency list of all words in ALL CORPORA!")
    freqlist_df <- dplyr::group_by(freqlist_df, .data$w)
    freqlist_df <- dplyr::summarise(freqlist_df, !!dplyr::quo_name(n_w_in_corp) := sum(.data$n))
  } else {
    colloc_df <- dplyr::group_by(colloc_df, .data$node, .data$corpus_names)
    freqlist_df <- dplyr::rename(freqlist_df, !!dplyr::quo_name(n_w_in_corp) := .data$n)
  }

  # if window_span is NULL, user intends to use all collocates span
  if (purrr::is_null(window_span)) {
    colloc_freq_df <- dplyr::count(colloc_df, .data$w, sort = TRUE)
    colloc_freq_df <- dplyr::arrange(colloc_freq_df, .data$node, dplyr::desc(.data$n))
    colloc_freq_df <- dplyr::rename(colloc_freq_df, !!dplyr::quo_name(a) := .data$n)
  } else {
    colloc_freq_df <- dplyr::filter(colloc_df, .data$span %in% window_span)
    colloc_freq_df <- dplyr::count(colloc_freq_df, .data$w, sort = TRUE)
    colloc_freq_df <- dplyr::arrange(colloc_freq_df, .data$node, dplyr::desc(.data$n))
    colloc_freq_df <- dplyr::rename(colloc_freq_df, !!dplyr::quo_name(a) := .data$n)
  }

  # check if stopwords removed from the calculation of collocation strength
  if(!purrr::is_null(stopword_list)) {
    colloc_freq_df <- dplyr::filter(colloc_freq_df, !.data$w %in% stopword_list)
    freqlist_df <- dplyr::filter(freqlist_df, !.data$w %in% stopword_list)
    message("You chose to remove stopwords!")
  } else {
    message("Stopwords are retained!")
  }

  # get the total corpus size
  if (per_corpus == FALSE) {
    corpussize_df <- unname(unlist(dplyr::tally(freqlist_df, wt = .data$n_w_in_corp)))
  } else {
    freqlist_df <- dplyr::group_by(freqlist_df, .data$corpus_names)
    corpussize_df <- dplyr::summarise(freqlist_df, !!dplyr::quo_name(corpus_size) := sum(.data$n_w_in_corp))
  }

  # get the total frequency of the search pattern
  if (length(search_pattern) > 1L) {

    # search pattern using regex with word boundary --------
    # search_pattern_id <- stringr::str_c(search_pattern, collapse = "|")
    # search_pattern_id <- stringr::str_c("(", search_pattern_id, ")", sep = "")

    # search pattern using exact pattern --------
    search_pattern_exact <- stringr::str_replace_all(search_pattern, "\\\\b(.+)\\\\b", "^\\1$")
    search_pattern_id <- stringr::str_c(search_pattern_exact, collapse = "|")
    search_pattern_id <- stringr::str_c("(", search_pattern_id, ")", sep = "")

  } else {

    ## search pattern using regex with word boundary ----------
    # search_pattern_id <- search_pattern

    ## search pattern using exact pattern ----------
    search_pattern_id <- stringr::str_replace_all(search_pattern, "\\\\b(.+)\\\\b", "^\\1$")
  }
  npattern_df <- dplyr::filter(freqlist_df, stringr::str_detect(.data$w, search_pattern_id))
  npattern_df <- dplyr::rename(npattern_df,
                               !!dplyr::quo_name(n_pattern) := .data$n_w_in_corp,
                               !!dplyr::quo_name(node) := .data$w)

  # generate an association table data base
  if (per_corpus == FALSE) {
    assoc_tb <- dplyr::left_join(colloc_freq_df, freqlist_df, by = "w")
    assoc_tb$corpus_size <- corpussize_df
    assoc_tb <- dplyr::left_join(assoc_tb, npattern_df, by = "node")
  } else {
    assoc_tb <- dplyr::left_join(colloc_freq_df, freqlist_df, by = c("w", "corpus_names"))
    assoc_tb <- dplyr::left_join(assoc_tb, corpussize_df, by = "corpus_names")
    assoc_tb <- dplyr::left_join(assoc_tb, npattern_df, by = c("node", "corpus_names"))
  }

  # prepare quoted variable for mutate variables
  b <- dplyr::quo(b)
  c <- dplyr::quo(c)
  d <- dplyr::quo(d)
  assoc <- dplyr::quo(assoc)
  w <- dplyr::quo(w)
  a_exp <- dplyr::quo(a_exp)

  # the design of the 2-by-2 table
  #
  # |          | CxN/Node   | others |
  # | -------- | ---------- | ------ |
  # | Collex   |  a         |   b    | n_w_in_corp
  # | others   |  c         |   d    | ...
  # | -------- | -----------| ------ |
  # |          | n_pattern  |   ...  | corpus_size


  # get the frequency for the contigency table inputs
  assoc_tb <- dplyr::mutate(assoc_tb,
                            !!dplyr::quo_name(b) := .data$n_w_in_corp - .data$a,
                            !!dplyr::quo_name(c) := .data$n_pattern - .data$a,
                            !!dplyr::quo_name(d) := .data$corpus_size - (.data$a + .data$b + .data$c))


  if (per_corpus == FALSE) {
    nested_assoc_tb <- tidyr::nest(dplyr::group_by(assoc_tb, !!w, !!node))
  } else {
    nested_assoc_tb <- tidyr::nest(dplyr::group_by(assoc_tb, !!w, !!node, !!corpus_names))
  }

  nested_assoc_tb <- dplyr::mutate(nested_assoc_tb, !!dplyr::quo_name(a_exp) := purrr::map_dbl(data, exp_freq, float_digits))
  assoc_tb <- tidyr::unnest(nested_assoc_tb)

  # add association direction
  assoc_tb <- dplyr::mutate(assoc_tb,
                            !!dplyr::quo_name(assoc) := "neutral",
                            !!dplyr::quo_name(assoc) := replace(.data$assoc, .data$a > .data$a_exp, "attraction"),
                            !!dplyr::quo_name(assoc) := replace(.data$assoc, .data$a < .data$a_exp, "repulsion"))

  # nest the data columns required for row-wise FYE with purrr map
  if (per_corpus == FALSE) {
    nested_assoc_tb <- tidyr::nest(dplyr::group_by(assoc_tb, !!w, !!node))
  } else {
    nested_assoc_tb <- tidyr::nest(dplyr::group_by(assoc_tb, !!w, !!node, !!corpus_names))
  }
  return(nested_assoc_tb)

} ## end of assoc_prepare()
