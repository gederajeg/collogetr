#' Generate window-span collocates for the Leipzig Corpora
#'
#' @description The function produces tibble-output collocates for the Leipzig corpus files.
#' @param leipzig_path Character strings of (i) file names of the Leipzig corpus if they are in the working directory, or (ii) the complete file path to each of the Leipzig corpus files.
#' @param leipzig_corpus_list Specify this argument if each Leipzig corpus file has been loaded as R object and acts as an element of a \bold{named list}.
#'     Example of this type of data-input can be seen in \code{data("demo_corpus_leipzig")}.
#'     So specify either \code{leipzig_path} OR \code{leipzig_corpus_list} and set one of them to \code{NULL}.
#' @param pattern Character vector input containing a set of exact word forms.
#' @param case_insensitive Logical; whether the search for the \code{pattern} ignores case (\code{TRUE} -- default) or not (\code{FALSE}).
#' @param window Character; window-span direction of the collocates: \code{"r"} ('\bold{right} of the node'), \code{"l"} ('\bold{left} of the node'), or the default is \code{"b"} ('both \bold{left} and \bold{right} context-window').
#' @param span An integer vector indicating the span of the collocate scope. The default is \code{2L}.
#' @param split_corpus_pattern Regular expressions used to tokenise the corpus into word-vector.
#'     The default regex is \code{"([^a-zA-Z-\u00AC]+|--)"}. The character \code{"\u00AC"} is a hexademical version of \code{"¬"}, which may occur in the Leipzig Corpora as separator between root and suffixes of a word, in addition to hypen.
#'     This procedure supports the vectorised method of the function to generate the collocate of the search pattern.
#' @param to_lower_colloc Logical; whether to lowercase the retrieved collocates and the nodes (\code{TRUE} -- default) or not (\code{FALSE}).
#' @param save_interim Logical; whether to save interim results into plain text files or not (\code{FALSE} -- default).
#' @param freqlist_output_file Character strings for the name of the file for the word frequency in a corpus.
#' @param colloc_output_file Character strings for the name of the file for the raw collocate table.
#' @param corpussize_output_file Character strings for the name of the file for the total word-size of a corpus.
#' @param search_pattern_output_file Character strings for the name of the file for the search_pattern.
#'
#' @return List of raw collocate items, frequency list of all words in the loaded corpus files, the total word tokens in each loaded corpus, and the search pattern.
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
#' @importFrom dplyr tally
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom tidyr gather
#' @importFrom readr read_lines
#' @importFrom readr write_tsv
#' @importFrom purrr is_null
#' @importFrom purrr map
#' @importFrom purrr pmap
#' @importFrom purrr map_dbl
#' @importFrom purrr map_int
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
#' @export
#'
#' @examples
#' \dontrun{
#' # prepare the leipzig file path and search patterns
#' my_leipzig_path <- c("/Users/Primahadi/Documents/ind_news_2008_300K-sentences.txt",
#'                      "/Users/Primahadi/Documents/ind_news_2009_300K-sentences.txt")
#' # the input pattern accepts exact word forms (e.g., c("menjalani", "menjalankan"))
#' my_pattern <- c("menjalani", "menjalankan")
#'
#' # (1) run the function but output the results in console ()
#' collout <- colloc_leipzig(leipzig_path = my_leipzig_path,
#'                            pattern = my_pattern,
#'                            window = "r", # get right side collocates
#'                            span = 3L, # for 'three' words to the right of the node/pattern
#'                            save_interim = FALSE # don't save into a file
#'                            )
#' # check the content of the output list
#' names(collout)
#' str(collout)
#'
#' # (2) run the function but save interim results per loaded corpus
#' outfiles <- colloc_leipzig(leipzig_path = my_leipzig_path,
#'                            pattern = my_pattern,
#'                            window = "r",
#'                            span = 3L,
#'                            save_interim = TRUE # save interim results to disk
#'                            freqlist_output_file = "~/Desktop/out_1_freqlist.txt",
#'                            colloc_output_file = "~/Desktop/out_2_collocates.txt",
#'                            corpussize_output_file = "~/Desktop/out_3_corpus_size.txt",
#'                            search_pattern_output_file = "~/Desktop/out_4_search_pattern.txt"
#'                            )
#' }
#'
#'
colloc_leipzig <- function(leipzig_path = NULL,
                           leipzig_corpus_list = NULL,
                           pattern = NULL,
                           case_insensitive = TRUE,
                           window = "b",
                           span = 2L,
                           split_corpus_pattern = "([^a-zA-Z-\u00AC]+|--)",
                           to_lower_colloc = TRUE,
                           save_interim = FALSE,
                           freqlist_output_file = "collogetr_out_1_freqlist.txt",
                           colloc_output_file = "collogetr_out_2_collocates.txt",
                           corpussize_output_file = "collogetr_out_3_corpus_size.txt",
                           search_pattern_output_file = "collogetr_out_4_search_pattern.txt") {

  # check the type of input selected by users: either 'leipzig_path' or 'leipzig_corpus_list'
  if (is.null(leipzig_path) & !is.null(leipzig_corpus_list)) { # if 'list' input
    message("Detecting a 'named list' input!")
    corpus_input <- leipzig_corpus_list
    corpus_input_type <- "list"

    # get corpus names
    corpus_names_all <- names(corpus_input)

  } else if (!is.null(leipzig_path) & is.null(leipzig_corpus_list)) { # if 'path' input
    message("Detecting a 'file path' input!")
    corpus_input <- leipzig_path
    corpus_input_type <- "path"

    # get corpus names
    corpus_names_all <- stringr::str_replace(basename(leipzig_path), '-sentences.*$', '')

  } else {
    stop("Requires corpus input! Either via 'leipzig_path' or 'leipzig_corpus_list'")
  }

  # check if there is pattern input
  if (purrr::is_null(pattern)) {
    stop("Requires input for the 'pattern' argument!")
  } else {
    pattern <- pattern
  }

  # store the search pattern to be saved
  res_pattern <- pattern

  # define a function to generate a results holder
  results_holder <- function(input = NULL, input_names = NULL, mode = "list") {
    pholder <- vector(mode = mode, length = length(input))
    names(pholder) <- input_names
    return(pholder)
  }

  # check if user wants to save interim results
  if (save_interim) { # TRUE

    message("Generating output files for storing interim results!")
    # output file template for freqlist
    table_header_freqlist <- stringr::str_c("corpus_names\tw\tn", sep = "")
    cat(table_header_freqlist, file = freqlist_output_file, sep = "\n", append = FALSE)

    # output file template for collocates table
    table_header_collocs <- stringr::str_c("corpus_names\tsent_id\tsent_elements\tw_vector_pos\tw\tspan\tnode\tsent_match", sep = "")
    cat(table_header_collocs, file = colloc_output_file, sep = "\n", append = FALSE)

    # output file template for corpus size
    table_header_corpsize <- stringr::str_c("corpus_names\tsize", sep = "")
    cat(table_header_corpsize, file = corpussize_output_file, sep = "\n", append = FALSE)

  } else { # FALSE -- not save interim

    message("You chose NOT to SAVE INTERIM RESULTS, which will be stored as a list in console!")

    # prepare a list to hold all results when not saved
    res_freqlist <- results_holder(input = corpus_input, input_names = corpus_names_all)
    res_colloc <- results_holder(input = corpus_input, input_names = corpus_names_all)
    res_corpussize <- results_holder(input = corpus_input, input_names = corpus_names_all)

  } # end of 'save interim' if else statement

  # vector for storing detected pattern in all loaded corpora
  detected_pattern_all_corpus <- vector(mode = "list", length = length(corpus_input))

  for (c in seq_along(corpus_input)) {

    # check the type of corpus input then load the corpus file
    if (corpus_input_type == "path") {
      corpora <- readr::read_lines(file = corpus_input[c])
    } else {
      corpora <- corpus_input[[c]]
    }

    # get corpus names
    corpus_names <- corpus_names_all[c]

    # vector to store which search pattern(s) is detected in the loaded corpus
    detected_pattern <- vector()

    # check if the search pattern can be found in the loaded corpus
    for (pp in seq_along(pattern)) {

      # define regex and exact pattern
      regex_pattern <- stringr::regex(pattern[pp], ignore_case = case_insensitive)

      # if the pattern does have word boundary character ("\\b")
      if (stringr::str_detect(pattern[pp], "\\\\b")) {

        exact_pattern <- stringr::str_replace_all(regex_pattern[1], "\\\\b(.+?)\\\\b", "^\\1$")

      } else if (stringr::str_detect(pattern[pp], "(\\^|\\$)")) {

        exact_pattern <- regex_pattern[1]

      } else {

        exact_pattern <- stringr::str_c("^", pattern[pp], "$", collapse = "")
      }

      # check if regex pattern is designed as "^....$"; if it is simply a word form (e.g., "mau"), assume that it is a whole word, hence add the word boundary "\\b"
      if (stringr::str_detect(regex_pattern[1], "(\\^|\\$)")) {

        pattern_to_detect <- stringr::str_replace_all(pattern[pp], "(\\^|\\$)", "\\b")
        regex_pattern <- stringr::regex(pattern_to_detect, ignore_case = case_insensitive)

      } else if (!stringr::str_detect(regex_pattern[1], "(\\^|\\$)")) {

        pattern_to_detect <- stringr::str_c("\\b", pattern[pp], "\\b", sep = "")
        regex_pattern <- stringr::regex(pattern_to_detect, ignore_case = case_insensitive)

      } else if (stringr::str_detect(regex_pattern[1], "\\b")) {

        pattern_to_detect <- stringr::str_c("\\b", pattern[pp], "\\b", sep = "")
        regex_pattern <- stringr::regex(pattern_to_detect, ignore_case = case_insensitive)

      }

      # detect if any match is found
      if (any(stringr::str_which(corpora, pattern = regex_pattern))) {
        message(stringr::str_c("At least one match is detected for the input form '",
                               stringr::str_replace_all(exact_pattern[1], "\\^|\\$", ""),
                               "' in ",
                               corpus_names,
                               ".",
                               sep = ""))
        detected_pattern[pp] <- pattern[pp]
      } else {
        warning(stringr::str_c("No match is detected for the input form '",
                               stringr::str_replace_all(exact_pattern[1], "\\^|\\$", ""),
                               "' in ",
                               corpus_names,
                               ".",
                               sep = ""))
      }
    }

    # remove NAs from detected_pattern
    detected_pattern <- detected_pattern[!is.na(detected_pattern)]

    # check if no patterns are detected in a corpus
    if (length(detected_pattern) == 0L) {

      next # move to the next corpus!

    } else { # if at least one pattern is found, proceed!

      # define the corpus with sentence marker
      sent_marker <- stringr::str_c(rep("stcs", span), collapse = " ")
      corpora1 <- stringr::str_c(sent_marker, corpora, sent_marker)

      # define span setting function
      span_setting <- function(window = NULL, span = NULL) {
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

      # define the span set
      span_set <- span_setting(window = window, span = span)
      span_set_excl_node <- span_set[names(span_set) != "node"] # exclude span for the node

      # tokenising the corpus
      message("1. Tokenising the corpus into corpus word-vector...")
      corpus_token <- stringr::str_split(string = corpora1, pattern = split_corpus_pattern)
      names(corpus_token) <- stringr::str_c("s_", 1:length(corpus_token), "__", sep = "")
      message("    1.1 Vectorising the split corpus (it does take some time for 1M-sentences corpora!!!)...")
      corpus_token <- unlist(corpus_token)
      message("    1.2 Removing one-character tokens")
      corpus_token <- corpus_token[nchar(corpus_token) > 1L] # remove one-letter/character token
      corpus_token <- corpus_token[!stringr::str_detect(corpus_token, "^-+$")] # remove hypen
      corpus_token <- corpus_token[!stringr::str_detect(corpus_token, "^-([A-Za-z0-9]|[0-9]+)$")]
      corpus_token <- corpus_token[!stringr::str_detect(corpus_token, "^([A-Za-z]|[0-9]+)-$")]
      rm(corpora1)

      # store the corpus-vector into a tibble
      corpus_token_df <- tibble::tibble(corpus_names = corpus_names,
                                        sent_id = as.integer(stringr::str_extract(names(corpus_token), "(?<=^s_)\\d+")),
                                        sent_elements = as.integer(stringr::str_extract(names(corpus_token), "(?<=__)\\d+")),
                                        w = corpus_token,
                                        w_vector_pos = seq_along(corpus_token))
      rm(corpus_token)

      # lower-casing the word-tokens
      if (to_lower_colloc == TRUE) {
        message("    1.3 Lowercasing the corpus word-vector...")
        wlower <- stringr::str_to_lower(corpus_token_df$w)
        w <- dplyr::quo(w)
        corpus_token_df <- dplyr::mutate(corpus_token_df,
                                         !!dplyr::quo_name(w) := wlower)
        rm(wlower)
      }

      # define holder for the collocates per pattern
      temp_colloc <- results_holder(input = detected_pattern, input_names = detected_pattern)

      for (p in seq_along(detected_pattern)) {

        # define regex and exact pattern
        regex_pattern <- stringr::regex(detected_pattern[p], ignore_case = case_insensitive)

        # if the pattern does have word boundary character ("\\b")
        if (stringr::str_detect(detected_pattern[p], "\\\\b")) {

          exact_pattern <- stringr::str_replace_all(regex_pattern[1], "\\\\b(.+?)\\\\b", "^\\1$")

        } else if (stringr::str_detect(detected_pattern[p], "(\\^|\\$)")) {

          exact_pattern <- regex_pattern[1]

        } else {

          exact_pattern <- stringr::str_c("^", detected_pattern[p], "$", collapse = "")
        }

        # nodeword
        nodewords <- stringr::str_replace_all(exact_pattern, "\\^|\\$", "")

        # pull out the vector position of the node
        message_text <- stringr::str_c("2.", p, " Gathering the vector-position for '", nodewords, "' ...", sep = "")
        message(message_text)
        node_pos <- dplyr::pull(dplyr::filter(corpus_token_df,

                                              # filter nodeword using regex with word boundary --------
                                              # stringr::str_detect(.data$w, pattern = regex_pattern)),

                                              # filter nodeword using regex with exact pattern ---------
                                              stringr::str_detect(.data$w, pattern = exact_pattern)),
                                .data$w_vector_pos)

        # add node status to all words in the corpus
        is_node <- dplyr::quo(is_node)
        corpus_token_df <- dplyr::mutate(corpus_token_df,
                                         !!dplyr::quo_name(is_node) := dplyr::if_else(.data$w_vector_pos %in% node_pos,
                                                                                      1L,
                                                                                      0L))

        # get full sentence match
        sent_id <- unique(dplyr::pull(dplyr::filter(corpus_token_df, .data$is_node == 1L), .data$sent_id))
        sent_match <- corpora[sent_id]
        sent_match_df <- dplyr::tibble(sent_id, sent_match)

        # determine the vector position of the collocates and store them into a data frame
        colloc_pos <- sapply(node_pos, function(node_post) node_post + span_set_excl_node, simplify = TRUE)

        if (is.matrix(colloc_pos)) { # if the vector position is returned as matrix, proceed as follows
          colloc_pos <- as.data.frame(colloc_pos)
          colloc_pos$span <- rownames(colloc_pos)

        } else { # the following procedure applies to one-integer span, hence returning a vector for the vector position

          colloc_pos1 <- colloc_pos
          colloc_pos <- data.frame(span = as.integer(colloc_pos1),
                                   vars = stringr::str_c("V", 1:length(colloc_pos1)))
          colloc_pos <- tidyr::spread(colloc_pos, .data$vars, .data$span)
          rownames(colloc_pos) <- unique(names(colloc_pos1))
          colloc_pos$span <- rownames(colloc_pos)
        }

        colloc_pos <- dplyr::as_tibble(dplyr::select(colloc_pos, .data$span, dplyr::everything()))
        colloc_pos <- tidyr::gather(colloc_pos, key = "junk_column_var", value = "w_vector_pos", -.data$span)
        colloc_pos <- dplyr::select(colloc_pos, -.data$junk_column_var)

        # retrieve the collocates from the word vector by matching their vector position
        message_text <- stringr::str_c("3.", p, " Gathering the collocates for '", nodewords, "' ...", sep = "")
        message(message_text)
        colloc_pos <- dplyr::filter(colloc_pos,
                                    .data$w_vector_pos >= 1,
                                    .data$w_vector_pos <= dim(corpus_token_df)[1])
        colloc_df_unique <- dplyr::left_join(colloc_pos, corpus_token_df, by = "w_vector_pos")
        colloc_df_unique <- dplyr::filter(colloc_df_unique,
                                          !duplicated(.data$w_vector_pos), # get unique collocates
                                          .data$w != "stcs", # remove sentence boundaries
                                          .data$is_node == 0L # remove node collocates
        )

        # add the nodeword column
        node <- dplyr::quo(node)
        colloc_df_unique <- dplyr::mutate(colloc_df_unique,
                                          !!dplyr::quo_name(node) := nodewords)

        # re-arranging the columns order
        colloc_df_unique <- dplyr::select(colloc_df_unique,
                                          .data$corpus_names,
                                          .data$sent_id,
                                          # .data$sent_elements,
                                          # .data$w_vector_pos,
                                          .data$w,
                                          .data$span,
                                          .data$node)

        # join the sentence match with the collocate table
        colloc_df_unique <- dplyr::left_join(colloc_df_unique, sent_match_df, by = "sent_id")

        # insert the output to the temporary list
        temp_colloc[[p]] <- colloc_df_unique

      } # end of "p" loop for each search pattern

      # combine the collocates data for each identified pattern
      temp_colloc <- dplyr::bind_rows(temp_colloc)

      # generate freq list of the corpus
      freqlist <- dplyr::count(dplyr::filter(corpus_token_df, .data$w != "stcs"),
                               .data$corpus_names,
                               .data$w,
                               sort = TRUE)
      rm(corpus_token_df)

      # generate corpus size tibble
      corpus_size <- tibble::tibble(corpus_names = corpus_names,
                                    size = dplyr::pull(dplyr::tally(freqlist, .data$n),
                                                       .data$nn))

      if (save_interim) {

        # save collocates
        readr::write_tsv(temp_colloc, path = colloc_output_file, append = TRUE)

        # save freqlist
        readr::write_tsv(freqlist, path = freqlist_output_file, append = TRUE)

        # save corpus size
        readr::write_tsv(corpus_size, path = corpussize_output_file, append = TRUE)

      } else {

        # insert the collocates into list
        res_colloc[[c]] <- dplyr::bind_rows(temp_colloc)

        # insert the freqlist into list
        res_freqlist[[c]] <- freqlist

        # insert the corpus size into list
        res_corpussize[[c]] <- corpus_size
      }

    } # end of "if (length(detected_pattern) == 0L)"

    rm(corpora)
    detected_pattern_all_corpus[[c]] <- detected_pattern

  } # end of "c" loop for each corpus file



  # save the search pattern
  if (save_interim) {
    cat("SEARCH_PATTERN", file = search_pattern_output_file, sep = "\n", append = TRUE)
    cat(res_pattern, file = search_pattern_output_file, sep = "\n", append = FALSE)
  }

  if (any(purrr::map_int(detected_pattern_all_corpus, length)) > 0L) {

    if (save_interim == FALSE) {

      # prepare all output data when not saving into disk
      message("4. Storing all of the outputs...")
      res_colloc <- dplyr::bind_rows(res_colloc)
      res_freqlist <- dplyr::bind_rows(res_freqlist)
      res_corpussize <- dplyr::bind_rows(res_corpussize)
      output_all <- list(colloc_df = res_colloc,
                         freqlist_df = res_freqlist,
                         corpussize_df = res_corpussize,
                         pattern = res_pattern)
      message("\nDONE!\n")
      return(output_all)

    } else {

      # message for finish processing all loaded corpora and saving interim results!
      message("\nReturning the names of the saved files!")
      output_file_names <- c(freqlist_output_file,
                             colloc_output_file,
                             corpussize_output_file,
                             search_pattern_output_file)
      message("\nDONE!\n")
      return(output_file_names)
    }

  } else {

    # message for non-match result in all corpora
    warning("\nSORRY!\nNot a single match is found for your search pattern in all the loaded corpora!")
  }

} # end of "colloc_leipzig()"
