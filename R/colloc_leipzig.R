#' Generate window-span collocates for the Leipzig Corpora
#'
#' @description The function produces tibble-output collocates for the Leipzig corpus files.
#' @param leipzig_path Character strings of (i) file names of the Leipzig corpus if they are in the working directory, or (ii) the complete file path to each of the Leipzig corpus files.
#' @param leipzig_corpus_list Specify this argument if each Leipzig corpus file has been loaded as R object and acts as an element of a \bold{named list}.
#'     Example of this type of data-input can be seen in \code{data("demo_corpus_leipzig")}.
#'     So specify either \code{leipzig_path} OR \code{leipzig_corpus_list} and set one of them to \code{NULL}.
#' @param pattern Regular expressions/exact patterns for the target pattern.
#' @param window Character; window-span direction of the collocates: \code{"r"} ('\bold{right} of the node'), \code{"l"} ('\bold{left} of the node'), or the default is \code{"b"} ('both \bold{left} and \bold{right} context-window').
#' @param span Integer vector indicating the span of the collocate scope.
#' @param case_insensitive Logical; whether the search for the \code{pattern} ignores case (\code{TRUE} -- default) or not (\code{FALSE}).
#' @param split_corpus_regex Regular expressions used to tokenise the corpus into word-vector.
#'     This procedure supports the vectorised method of the function to generate the collocate of the search pattern.
#' @param to_lower_colloc Logical; whether to lowercase the retrieved collocates and the nodes (\code{TRUE} -- default) or not (\code{FALSE}).
#' @param save_interim Logical; whether to save interim results into plain text files or not (\code{FALSE} -- default).
#' @param colloc_output_name Character strings for the name of the file for the raw collocate table.
#' @param colloc_freq_output_name Character strings for the name of the file for the collocate frequency table.
#' @param freqlist_output_name Character strings for the name of the file for the word frequency in a corpus.
#' @param sentence_output_name Character strings for the name of the file for the full sentence match containing the collocates.
#' @param corpus_size_output_name Character strings for the name of the file for the total word-size of a corpus.
#' @return List of raw collocate items, collocates frequency, frequency list of all words in the corpus, the summed size of all corpus, the search regex-patterns.
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr count
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom readr read_lines
#' @importFrom readr write_tsv
#' @importFrom purrr is_null
#' @importFrom purrr map
#' @importFrom purrr pmap
#' @importFrom purrr map_dbl
#' @importFrom purrr map_chr
#' @importFrom purrr set_names
#' @importFrom stringr regex
#' @importFrom stringr str_c
#' @importFrom stringr str_count
#' @importFrom stringr str_which
#' @importFrom stringr str_detect
#' @importFrom stringr str_length
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_locate_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_trim
#' @importFrom stringr str_sub
#' @importFrom stringr str_split
#' @importFrom stringr str_subset
#' @importFrom stringr str_to_lower
#' @importFrom dplyr %>%
#' @importFrom dplyr quo_name
#' @importFrom dplyr quo
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @examples
#' out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig[1:4],
#'                       pattern = "\\bke\\b",
#'                       window = "r",
#'                       span = 1,
#'                       save_interim = FALSE)
#' @export

colloc_leipzig <- function(leipzig_path = NULL,
                           leipzig_corpus_list = NULL,
                           pattern = NULL,
                           window = "b",
                           span = 2,
                           case_insensitive = TRUE,
                           split_corpus_regex = "([^a-zA-Z-]+|--)",
                           to_lower_colloc = TRUE,
                           save_interim = FALSE,
                           colloc_output_name = "df_colloc.txt",
                           colloc_freq_output_name = "df_colloc_freq.txt",
                           freqlist_output_name = "df_freqlist.txt",
                           sentence_output_name = "df_sentence_match.txt",
                           corpus_size_output_name = "df_corpus_size.txt") {

  # check the specified argument for the corpus input
  corpus_input_check <- check_file_input(path_input = leipzig_path, list_input = leipzig_corpus_list)
  corpus_input <- corpus_input_check[["corpus_input"]]
  corpus_names <- corpus_input_check[["corpus_names"]]

  # define the placeholder for the output
  all_words_freq <- placeholder(corpus_input, corpus_names, "list")
  all_collocs <- placeholder(corpus_input, corpus_names, "list")
  all_corpus_size <- placeholder(corpus_input, corpus_names, "list")
  match_id_all <- placeholder(corpus_input, corpus_names, "list")
  # all_match_sent <- placeholder(corpus_input, corpus_names, "list")

  # define the placeholder for the saved output if specified
  if (save_interim == TRUE) {

  save_output_file(raw_colloc = colloc_output_name,
                   freq_colloc = colloc_freq_output_name,
                   freqlist = freqlist_output_name,
                   corpussize = corpus_size_output_name,
                   sent_match = NULL)

  }

  # i=1

  for (i in seq_along(corpus_input)) {

    # check the type of input object
    corpora <- check_type_input(input = corpus_input, input_names = corpus_names, indices = i)

    # define regex and exact pattern
    regex_pattern <- stringr::regex(pattern, ignore_case = case_insensitive)
    exact_pattern <- stringr::str_replace_all(regex_pattern[1], "\\\\b(.+?)\\\\b", "^\\1$")

    # detect if any match is found
    match_id <- stringr::str_which(corpora, pattern = regex_pattern)
    match_id_all[[i]] <- match_id

    # skip if no match is found for a given corpus
    if (length(match_id) == 0L) {

      message("No match found! We move to the next corpus...\n")
      next

    } else {

      message(paste(length(match_id), " matches are detected!", sep = ""))

    } # end of else 'match_id'
      # match_sent <- stringr::str_replace_all(corpora[match_id], "([^a-zA-Z0-9- ]+)", " \\1 ")
      # match_sent <- stringr::str_trim(stringr::str_replace_all(match_sent, "\\s{2,}",  " "))
      # match_length <- stringr::str_count(match_sent, pattern = regex_pattern)
      # match_sent <- rep(match_sent, match_length)
      # match_id <- rep(match_id, match_length)
      # match_sent_df <- tibble::as_tibble(data.frame(corpus_names = corpus_names[i],
      #                                              sent_id = match_id,
      #                                              sent_match = match_sent,
      #                                              stringsAsFactors = FALSE,
      #                                              row.names = NULL))

      # define the corpus with sentence marker
      sent_marker <- paste(rep("SENT", span), collapse = " ")
      corpora <- paste(sent_marker, corpora, sent_marker)

      # split the corpus into word-vector
      message("1. Tokenising the corpus into corpus word-vector...")
      corp_vect <- tokenising(corpora, split_corpus_regex, to_lower_colloc, corpus_names[i])

      # define the span vector-set
      span_set <- span_set(window = window, span = span)
      span_set_excl_node <- span_set[names(span_set) != "node"]

      # define a tibble storing all word vector
      corp_vect_df <- word_vect_to_df(tokenised_text = corp_vect)

      # pull out the vector position of the node
      message("2. Gathering the vector-position of the search pattern...")
      node_pos <- node_vect_pos(df = corp_vect_df, regex_pattern = regex_pattern)

      # add node status to all words in the corpus
      corp_vect_df <- dplyr::mutate(corp_vect_df,
                                    node_status = dplyr::if_else(.data$w_vector_pos %in% node_pos,
                                                                                                 "yes",
                                                                                                 "no"))

      # determine the vector position of the collocates and store them into a data frame
      colloc_pos <- sapply(node_pos, function(node_post) node_post + span_set_excl_node, simplify = TRUE)
      colloc_pos <- unique(as.vector(colloc_pos))

      # if (is.matrix(colloc_pos) == FALSE) {
      #
      #   # operation for colloc_pos of one dimensional vector
      #   coll_pos_names <- names(colloc_pos)
      #   colloc_pos <- unname(colloc_pos)
      #   coll_pos_names <- paste(coll_pos_names, "_", 1:length(colloc_pos), sep = "")
      #   names(colloc_pos) <- coll_pos_names
      #
      # } else {
      #
      #   # operation for colloc_pos of matrix
      #   colloc_pos <- data.frame(t(colloc_pos))
      #   colnames(colloc_pos) <- stringr::str_replace(colnames(colloc_pos), "^([a-z])([0-9]+)$", "\\1\\2_")
      #   colloc_pos <- sort(unlist(colloc_pos), decreasing = FALSE)
      #
      # }

      # colloc_pos_df <- tibble::as_tibble(data.frame(w_vector_pos = colloc_pos,
      #                                               # w_span = names(colloc_pos),
      #                                               stringsAsFactors = FALSE,
      #                                               row.names = NULL))
      # colloc_pos_df <- dplyr::filter(colloc_pos_df,
      #                                .data$w_vector_pos >= 1,
      #                                .data$w_vector_pos <= dim(corp_vect_df)[1])

      # retrieve the collocates from the word vector by matching their vector position
      message("3. Gathering the collocates of the search pattern...")
      colloc_pos <- colloc_pos[colloc_pos >= 1 & colloc_pos <= dim(corp_vect_df)[1]]
      colloc_df_unique <- dplyr::filter(corp_vect_df, .data$w_vector_pos %in% colloc_pos) %>%
        dplyr::filter(.data$w != "SENT") # %>%
        # dplyr::select(-.data$w_span)
      # colloc_df_unique <- unique(colloc_df_unique)

      # remove sentence marker from the word vector
      corp_vect_df <- dplyr::filter(corp_vect_df, .data$w != "SENT")

      # colloc_pos <- colloc_pos[colloc_pos >= 1 & colloc_pos <= length(corp_vect)]
      # colloc_types <- corp_vect[colloc_pos]
      # colloc_types <- colloc_types[colloc_types != "SENT"]
      # all_collocs[[i]] <- colloc_types
      # all_words[[i]] <- corp_vect[corp_vect != "SENT"]

      # store the tables for corpus vector and the collocates into a list
      message("4. Storing the output...")
      all_collocs[[i]] <- dplyr::filter(colloc_df_unique, .data$node_status == "no") %>%
        dplyr::select(-.data$node_status,
                      -.data$w_vector_pos,
                      -.data$sent_elements)
      all_words_freq[[i]] <- dplyr::count(corp_vect_df, .data$w, sort = TRUE)
      all_corpus_size[[i]] <- length(corp_vect[corp_vect != "SENT"])
      # all_match_sent[[i]] <- match_sent_df

      # i = i + 1

    # check if interim result needs to be saved
    if (save_interim == TRUE) {

      # generate data frame for the corpus total size
      corpus_size_df <- data.frame(corpus_names = names(all_corpus_size)[i],
                                   size = all_corpus_size[[i]],
                                   stringsAsFactors = FALSE,
                                   row.names = NULL)

      # generate data frame for the collocates frequency
      colloc_freq_df <- dplyr::count(all_collocs[[i]], .data$corpus_names, .data$w, sort = TRUE)
      colnames(colloc_freq_df)[grep("^n$", colnames(colloc_freq_df), perl = TRUE)] <- "a"

      # save the output into tab-separated plain text files
      readr::write_tsv(x = all_collocs[[i]], path = colloc_output_name, append = TRUE)
      readr::write_tsv(x = all_words_freq[[i]], path = freqlist_output_name, append = TRUE)
      # readr::write_tsv(x = all_match_sent[[i]], path = sentence_output_name, append = TRUE)
      readr::write_tsv(x = corpus_size_df, path = corpus_size_output_name, append = TRUE)
      readr::write_tsv(x = colloc_freq_df, path = colloc_freq_output_name, append = TRUE)

      # completed message
      message(paste('5. Interim results from "', corpus_names[i], '" have been saved!', sep = ""))
      message(paste('DONE with "', corpus_names[i], '"!\n', sep = ""))

    } else {

      # completed message
      message(paste('DONE with "', corpus_names[i], '"!\n', sep = ""))

    } # end of else 'save_interim'

    rm(colloc_pos)

  } # end of i

  if (all(sapply(match_id_all, length) == 0L) == FALSE) {

    collocs_df <- dplyr::bind_rows(all_collocs)
    collocs_freq <- dplyr::count(collocs_df, .data$w, sort = TRUE)
    colnames(collocs_freq)[grep("^n$", colnames(collocs_freq), perl = TRUE)] <- "a"
    words_freq <- dplyr::bind_rows(all_words_freq)
    words_freq <- dplyr::group_by(words_freq, .data$w) %>%
      dplyr::summarise(n_w_in_corp = sum(.data$n)) %>%
      dplyr::arrange(dplyr::desc(.data$n_w_in_corp))
    all_corpus_size <- sum(unlist(all_corpus_size))
    # match_sent <- dplyr::bind_rows(all_match_sent)

    output <- list(collocs_df = collocs_df,
                   collocs_freq = collocs_freq,
                   words_freq = words_freq,
                   all_corpus_size = all_corpus_size,
                   node_regex = regex_pattern,
                   node_regex_exact = exact_pattern)
    return(output)
  } else {
    message("No match found in all corpus inputs!")
  }
} # end of the function
