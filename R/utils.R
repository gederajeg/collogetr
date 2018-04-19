#' Verify the input file
#'
#' @description Utility function to check whether \code{colloc_leipzig} receive a file path input or a list-corpus input.
#'     It is called internally by \code{colloc_leipzig}.
#' @param path_input Receive input from the \code{leipzig_path} argument of \code{colloc_leipzig}.
#' @param list_input Receive input from the \code{leipzig_corpus_list} argument of \code{colloc_leipzig}.
#' @importFrom purrr is_null
#' @importFrom stringr str_c
#' @importFrom stringr str_replace
check_file_input <- function(path_input = NULL, list_input = NULL) {
  if (purrr::is_null(path_input) & !purrr::is_null(list_input)) {

    # corpus input as a list of text
    corpus_input <- list_input

    # get the corpus names
    corpus_names <- names(list_input)

    if (purrr::is_null(corpus_names)) {
      corpus_names <- stringr::str_c("corpus_list_element_",
                                     seq_along(list_input), sep = "")
    }

  } else if (!purrr::is_null(path_input) & purrr::is_null(list_input)) {

    # corpus input as filepath
    corpus_input <- path_input

    # corpus names
    corpus_names <- stringr::str_replace(basename(corpus_input), '-sentences.*$', '')

  } # end of the check corpus input type

  output <- list(corpus_input = corpus_input,
                 corpus_names = corpus_names)

  return(output)
}

#' Check input type
#'
#' @description Utility function to check type of object for the input of \code{colloc_leipzig}.
#'     It is called internally by \code{colloc_leipzig}.
#' @param input Either a character vector of Leipzig Corpora file path or a list whose elements correspond to the Leipzig Corpora File loaded into R.
#' @param input_names Names of the corpus file
#' @param indices For-loop index within \code{colloc_leipzig}
#' @return A character vector of corpus sentences.
#' @importFrom readr read_lines
check_type_input <- function(input = NULL, input_names = NULL, indices = NULL) {

  if (typeof(input) == "character") {

    # read in the corpus text if it is a filepath
    message('Detecting a "file path" input for corpora!')
    corpora <- readr::read_lines(file = input[indices])
    message(paste('"', basename(input[indices]), '" ', "has been loaded!", sep = ""))
    return(corpora)

  } else if (typeof(input) == "list") {

    corpora <- input[[indices]]
    message('Detecting a "list" input of corpora!')
    message(paste('Processing "', input_names[indices], '"...', sep = ""))
    return(corpora)

  }

}

#' Gather the node-vector
#'
#' @description Utility function to determine the vector-position of the search pattern/node word for which the collocates to be extracted.
#'     It is called internally by \code{colloc_leipzig}
#' @param df The tibble from the output of \code{word_vect_to_df}.
#' @param regex_pattern Passed on from the regular expression specified into \code{pattern} argument of \code{colloc_leipzig}.
#'
#' @return An integer vector.
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom stringr str_detect
#' @importFrom rlang .data
node_vect_pos <- function(df = NULL, regex_pattern = NULL) {
  out_vect <- dplyr::pull(dplyr::filter(df,
                                        stringr::str_detect(.data$w, pattern = regex_pattern)),
                          .data$w_vector_pos)
  return(out_vect)
}

#' Output placeholder
#'
#' @description Utility function to generate the placeholder/object for the output elements of \code{colloc_leipzig}.
#'     It is called internally by \code{colloc_leipzig}.
#' @param input The input object verified via another utility function, namely \code{check_file_input}.
#' @param input_names Names of the corpus file
#' @param mode Type of object for the placeholder. By default, it is \code{"list"}.
#'
#' @return empty list for the results of \code{colloc_leipzig}.
placeholder <- function(input = NULL, input_names = NULL, mode = "list") {
  pholder <- vector(mode = mode, length = length(input))
  names(pholder) <- input_names
  return(pholder)
}

#' Saving outputs
#'
#' @description Utility function to generate the output file of \code{colloc_leipzig} when the \code{save_results} argument is set to \code{TRUE}.
#'     It is called internally by \code{colloc_leipzig}.
#' @param raw_colloc File name for the raw collocates table.
#'     It is passed on from \code{colloc_output_name} argument of \code{colloc_leipzig}.
#' @param freq_colloc File name for the collocates frequency table.
#'     It is passed on from \code{colloc_freq_output_name} argument of \code{colloc_leipzig}.
#' @param freqlist File name for the frequency list table of all word tokens in the corpus.
#'     It is passed on from \code{freqlist_output_name} argument of \code{colloc_leipzig}.
#' @param corpussize File name for the overall corpus size in word-tokens.
#'     It is passed on from \code{corpus_size_output_name} argument of \code{colloc_leipzig}.
#' @param sent_match File name for the sentence match in which the \code{pattern} argument is detected.
#'     It is passed on from \code{sentence_output_name} argument of \code{colloc_leipzig}.
#' @return Tab-separated plain texts corresponding to the specified output results.
save_output_file <- function(raw_colloc = NULL,
                             freq_colloc = NULL,
                             freqlist = NULL,
                             corpussize = NULL,
                             sent_match = NULL) {

  # for the raw collocates
  cat(paste("corpus_names\tsent_id\tsent_elements\tw\tw_vector_pos\tnode_status", sep = ""),
      file = raw_colloc,
      sep = "\n",
      append = TRUE)

  # for the collocates frequency
  cat(paste("corpus_names\tw\ta", sep = ""), file = freq_colloc, sep = "\n", append = TRUE)

  # for the frequency list
  cat(paste("w\tn", sep = ""), file = freqlist, sep = "\n", append = TRUE)

  # for the corpus size
  cat(paste("corpus_names\tsize", sep = ""), file = corpussize, sep = "\n", append = TRUE)

  # for the sentence match
  # cat(paste("corpus_names\tsent_id\tsent_match", sep = ""), file = sent_match, sep = "\n", append = TRUE)
}


#' Define the integer vector for collocate windows
#'
#' @description Utility internal-function for constructing window-span vector in the collocate function.
#' @param window window-span direction of the collocates: \code{"r"} ('\bold{right} of the node'), \code{"l"} ('\bold{left} of the node'), or the DEFAULT is \code{"b"} ('both \bold{left} and \bold{right} context-window').
#' @param span integer vector indicating the span of the collocate scope.
#' @return Named integer vectors of window-span. The name attributes refer to the window position of the vector (e.g. "l2", "l1", "node", "r1", "r2").
#' @importFrom stringr str_c
span_set <- function(window = "character", span = "integer") {

  if (window == "l") {
    span1 <- span
    span <- -span:0
    names(span) <- c(stringr::str_c("l", span1:1, sep = ""), "node")
  } else if (window == "r") {
    span1 <- span
    span <- 0:span
    names(span) <- c("node", stringr::str_c("r", 1:span1))
  } else {
    span1 <- span
    span <- -span:span
    names(span) <- c(stringr::str_c("l", span1:1, sep = ""), "node", stringr::str_c("r", 1:span1, sep = ""))
  }
  return(span)
}

#' Tokenising the Leipzig Corpora
#'
#' @description Utility function to tokenise the input corpus of sentences into word vector.
#'     It is called internally by \code{colloc_leipzig}.
#' @param corpus A character vector of corpus sentences.
#' @param split_pattern A regular expression for how the corpus to be split.
#'     It is passed on from \code{split_corpus_regex} argument of \code{colloc_leipzig}.
#' @param lower_case Logical; whether to lower case the tokenised items or not.
#'     It is passed on from \code{to_lower_colloc} argument of \code{colloc_leipzig}.
#' @param corpus_names Name of the corpus file being processed.
#'
#' @return A large character vector of word-tokens.
tokenising <- function(corpus = NULL,
                       split_pattern = NULL,
                       lower_case = NULL,
                       corpus_names = NULL) {

  corp_vect <- stringr::str_split(corpus, split_pattern)
  message("    1.1 Renaming the tokens with sentence and corpus labels...")
  names(corp_vect) <- paste(corpus_names, "__sent_", 1:length(corp_vect), "__", sep = "")
  message("    1.2 Vectorising the tokens...")
  corp_vect <- unlist(corp_vect)

  # lowercasing if indicated
  if (lower_case == TRUE) {

    message("    1.3 Lowercasing the word-tokens...")
    corp_vect <- ifelse(corp_vect != "SENT",
                        stringr::str_to_lower(corp_vect),
                        corp_vect)
  }

  # remove one-letter words from the corpus word vector
  message("    1.4 Removing one-letter tokens...")
  corp_vect <- corp_vect[which(nchar(corp_vect) > 1)]
  corp_vect <- corp_vect[!stringr::str_detect(corp_vect, "^-+$")]
  corp_vect <- corp_vect[!stringr::str_detect(corp_vect, "^-[a-z]$")]
  corp_vect <- corp_vect[!stringr::str_detect(corp_vect, "^[a-z]-$")]

  return(corp_vect)

}

#' Data frame function for word-vector
#'
#' @description Utility function to turn the tokenised corpus into data frame of word-tokens.
#'     It is called internally by \code{colloc_leipzig}.
#' @param tokenised_text The output of \code{tokenising}.
#'
#' @return A tbl_df.
#' @importFrom stringr str_extract
#' @importFrom tibble as_tibble
word_vect_to_df <- function(tokenised_text = NULL) {
  out_df <- tibble::as_tibble(data.frame(corpus_names = stringr::str_extract(names(tokenised_text), "^.+?(?=__)"),
                                         sent_id = stringr::str_extract(names(tokenised_text), "(?<=__sent_)\\d+"),
                                         sent_elements = stringr::str_extract(names(tokenised_text), "(?<=__)\\d+$"),
                                         w = tokenised_text,
                                         w_vector_pos = seq_along(tokenised_text),
                                         stringsAsFactors = FALSE,
                                         row.names = NULL))
  return(out_df)
}

#' Fisher's Exact Test
#'
#' @description Perform one-tailed Fisher's Exact test for the collostruction/collocation strength.
#'     The \emph{p}-value is log-transformed to the base of ten as in the \emph{Collostructional Analysis}.
#'     This is an internal function called via \code{\link{collex_fye}}.
#' @param df The output of \code{\link{assoc_prepare}}.
#' @param mpfr_precision Integer indicating the maximal precision to be used in \bold{bits}. This is passed to the \code{precBits} argument of \code{\link[Rmpfr]{mpfr}}.
#' @param collstr_digit The floating digits of the collostruction strength. It is passed on from \code{\link{collex_fye}} and the default is \code{3}.
#'
#' @return A double vector of collostruction strength
#' @importFrom Rmpfr mpfr
#' @importFrom Rmpfr asNumeric
#' @importFrom base rbind
#'
#'
fye_compute <- function(df, mpfr_precision = NULL, collstr_digit = NULL) {

  # get into crosstabulation format
  crosstab <- base::rbind(c(df$a, df$b), c(df$c, df$d))

  # FYE computation
  if (!purrr::is_null(mpfr_precision)) { # if it requires MPFR precision floating point
    if (df$a > df$a_exp) {
      pval <- stats::fisher.test(crosstab, alternative = "greater")$p.value
      pval <- Rmpfr::asNumeric(Rmpfr::mpfr(pval, mpfr_precision))
      collstr <- round(-log10(pval), collstr_digit)
    } else {
      pval <- stats::fisher.test(crosstab, alternative = "less")$p.value
      pval <- Rmpfr::asNumeric(Rmpfr::mpfr(pval, mpfr_precision))
      collstr <- round(log10(pval), collstr_digit)
    }
  } else { # if MPFR precision is not required
    collstr <- dplyr::if_else(df$a > df$a_exp,
                              round(-log10(stats::fisher.test(crosstab, alternative = "greater")$p.value), collstr_digit),
                              round(log10(stats::fisher.test(crosstab, alternative = "less")$p.value), collstr_digit))
  }

  return(collstr)
}


#' Uni-directional measure \emph{Delta P} (Construction as the cue).
#'
#' @description Perform uni-directional association measure called \emph{Delta P}. This one measures how likely is the collocates/collexemes given the presence of the construction. That is, the construction/node word is the cue while the collocates/collexemes are the outcomes.
#'     The function is internally called when performing \code{\link{collex_fye}}.
#' @param df The output of \code{\link{assoc_prepare}}.
#' @param collstr_digit The floating digits of the collostruction strength. It is passed on from \code{\link{collex_fye}} and the default is \code{3}.
#'
#' @return A double numeric vector
dP_cue_collex <- function(df, collstr_digit = NULL) {
  output <- round(df$a/(df$a + df$b) - df$c/(df$c + df$d), collstr_digit)
  return(output)
}

#' Uni-directional measure \emph{Delta P} (Collocates as the cue).
#'
#' @description Perform uni-directional association measure called \emph{Delta P}. This one measures how likely is the construction/node word given the presence of the collocates/collexemes. That is, the collocates/collexemes are the cue while the construction/node word is the outcome.
#'     The function is internally called when performing \code{\link{collex_fye}}.
#' @param df The output of \code{\link{assoc_prepare}}.
#' @param collstr_digit The floating digits of the collostruction strength. It is passed on from \code{\link{collex_fye}} and the default is \code{3}.
#'
#' @return A double numeric vector
dP_cue_cxn <- function(df, collstr_digit = NULL) {
  output <- round(df$a/(df$a + df$c) - df$b/(df$b + df$d), collstr_digit)
  return(output)
}



# Perform Delta P
# dP.cxn.cue.collex <- round(a/(a + b) - c/(c + d), collstr.float.digit)
# dP.collex.cue.cxn <- round(a/(a + c) - b/(b + d), collstr.float.digit)

## Below are codes used to test-run the function
# span = 3
# window = "l"
# pattern = "\\bmemiliki\\b"
# split_corpus_regex = "([^a-zA-Z-]+)"
# case_insensitive = TRUE
# leipzig_path = NULL
# to_lower_colloc = TRUE
# leipzig_corpus_list = demo_corpus_leipzig
# save_interim = FALSE
# colloc_output_name = "df_colloc.txt"
# colloc_freq_output_name = "df_colloc_freq.txt"
# freqlist_output_name = "df_freqlist.txt"
# sentence_output_name = "df_sentence_match.txt"
# corpus_size_output_name = "df_corpus_size.txt"
# i=1
