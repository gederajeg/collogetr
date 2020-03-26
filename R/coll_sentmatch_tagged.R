
#' Sentence match retriever with tagged collocations
#'
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' @description This is an expanded function for \code{\link{colloc_sentmatch}} to retrieve sentence matches in which a given collocate is found.
#' The extended features in \code{\link{colloc_sentmatch_tagged}} (but not available in \code{\link{colloc_sentmatch}}) are:
#' \enumerate{
#'   \item a tibble/data frame output format (similar to the \code{colloc_df}, which is one of the outputs of \code{\link{colloc_leipzig}})
#'   \item tagging for the collocates (with \code{"<c>...</c>" tag}) and the nodes (\code{"<n>...</n>"}) in the sentence matches
#'   \item a column called \code{"coll_pattern"} containing the collocate-node pattern extracted via regular expression on the basis of the tagging.
#' }
#'
#' @param collout List output of \code{\link{colloc_leipzig}}.
#' @param colloc Character vector of the collocate(s) whose sentence match(es) to be retrieved.
#'
#' @return A data frame with the following variables/columns:
#' \itemize{
#'   \item \code{corpus_names} - corpus file name
#'   \item \code{sent_id} - sentence number of the collocate matches
#'   \item \code{w} - the collocate whose full sentence match with the node is retrieved via the function
#'   \item \code{span} - the window-span of the collocate in relation to the node word
#'   \item \code{node} - the node word
#'   \item \code{sent_match_tagged} - tagged sentence matches for the collocate of interest with the node
#'   \item \code{coll_pattern} - extracted collocate-node pattern from the sentence matches
#' }
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom purrr pmap_chr
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#' @importFrom dplyr quo
#' @importFrom dplyr quo_name
#' @importFrom dplyr tibble
#' @importFrom stringr str_locate_all
#' @importFrom purrr map_dfr
#' @importFrom dplyr bind_cols
#'
#' @seealso \code{\link{colloc_sentmatch}} for untagged and character-vector version of the output, \code{\link{colloc_leipzig}} for collocate retrieval.
#'
#' @examples
#' # retrieve the collocate of "sudah" 'already'
#' collout <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
#'                           pattern = "sudah",
#'                           window = "b",
#'                           span = 2,
#'                           save_interim = FALSE)
#'
#' # retrieve and tag the sentence match for "sudah" with its collocate "ada" 'exist'
#' df_sentmatch <- colloc_sentmatch_tagged(collout, colloc = "ada")
#'
#' # check the tagging in the sentence match
#' df_sentmatch$sent_match_tagged
#'
#' # check the extracted collocation pattern and the window span
#' df_sentmatch[, c("span", "coll_pattern")]
#'
colloc_sentmatch_tagged <- function(collout, colloc = NULL) {

  assertthat::assert_that(!is.null(colloc),
                          msg = "`colloc` argument cannot be NULL. Specify with one or more collocates!")

  df <- dplyr::filter(collout[[1]], .data$w %in% colloc)
  spanwindow <- df$span
  span_direction <- gsub("\\d", "", spanwindow, perl = TRUE)
  span_number <- as.numeric(gsub("\\D", "", spanwindow, perl = TRUE))
  spandf <- dplyr::tibble(span_direction, span_number)
  string <- dplyr::quo(string)
  pattern <- dplyr::quo(pattern)
  replacement <- dplyr::quo(replacement)
  nodes <- df$node
  sent_match <- df$sent_match
  spandf <- dplyr::mutate(spandf,
                          !!dplyr::quo(string) := sent_match,
                          !!dplyr::quo_name(pattern) := dplyr::if_else(.data$span_direction == "l",
                                                                       paste("((?i)", colloc, ")(([^a-zA-Z0-9-]+[a-zA-Z0-9-]+){", .data$span_number-1, "}[^a-zA-Z0-9-]+)((?i)", nodes, ")", sep = ""),
                                                                       paste("((?i)", nodes, ")(([^a-zA-Z0-9-]+[a-zA-Z0-9-]+){", .data$span_number-1, "}[^a-zA-Z0-9-]+)((?i)", colloc, ")", sep = "")),
                          !!dplyr::quo_name(replacement) := dplyr::if_else(.data$span_direction == "l",
                                                                           "<c>\\1</c>\\2<n>\\4</n>",
                                                                           "<n>\\1</n>\\2<c>\\4</c>"))
  spandf <- dplyr::mutate(spandf,
                          !!dplyr::quo_name(pattern) := dplyr::if_else(.data$span_number == 1 & .data$span_direction == "l",
                                                                       paste("((?i)", colloc, ")([^a-zA-Z0-9-]+)((?i)", nodes, ")", sep = ""),
                                                                       .data$pattern),
                          !!dplyr::quo_name(replacement) := dplyr::if_else(.data$span_number == 1 & .data$span_direction == "l",
                                                                           "<c>\\1</c>\\2<n>\\3</n>",
                                                                           .data$replacement),
                          !!dplyr::quo_name(pattern) := dplyr::if_else(.data$span_number == 1 & .data$span_direction == "r",
                                                                       paste("((?i)", nodes, ")([^a-zA-Z0-9-]+)((?i)", colloc, ")", sep = ""),
                                                                       .data$pattern),
                          !!dplyr::quo_name(replacement) := dplyr::if_else(.data$span_number == 1 & .data$span_direction == "r",
                                                                           "<n>\\1</n>\\2<c>\\3</c>",
                                                                           .data$replacement))
  spandf1 <- dplyr::select(spandf, 3:5)
  sent_tagged <- purrr::pmap_chr(spandf1, stringr::str_replace)
  coll_extract <- stringr::str_extract(sent_tagged, "<.+\\/.>")
  sent_match_tagged <- dplyr::quo(sent_match_tagged)
  coll_pattern <- dplyr::quo(coll_pattern)
  df <- dplyr::mutate(df,
                      !!dplyr::quo_name(sent_match_tagged) := sent_tagged,
                      !!dplyr::quo_name(coll_pattern) := coll_extract)
  if (any(is.na(df$coll_pattern))) {

    df <- dplyr::mutate(df,
                        !!dplyr::quo_name(sent_match_tagged) := dplyr::if_else(is.na(.data$coll_pattern) & stringr::str_detect(.data$span, "^l"),
                                                                               stringr::str_replace(sent_match_tagged,
                                                                                                    paste("((?i)", colloc, ")(([^a-zA-Z0-9-]+[a-zA-Z0-9-[:punct:]]+){", as.numeric(stringr::str_replace(.data$span, "\\D", "")), "}[^a-zA-Z0-9-]+)((?i)", nodes, ")", sep = ""),
                                                                                                    "<c>\\1</c>\\2<n>\\4</n>"),
                                                                               .data$sent_match_tagged),
                        !!dplyr::quo_name(sent_match_tagged) := dplyr::if_else(is.na(.data$coll_pattern) & stringr::str_detect(.data$span, "^l") & stringr::str_detect(.data$span, "1$"),
                                                                               stringr::str_replace(sent_match_tagged,
                                                                                                    paste("((?i)", colloc, ")(([^a-zA-Z0-9-]+[a-zA-Z0-9-[:punct:]]+){", as.numeric(stringr::str_replace(.data$span, "\\D", "")), "}[^a-zA-Z0-9-]+)((?i)", nodes, ")", sep = ""),
                                                                                                    "<c>\\1</c>\\2<n>\\3</n>"),
                                                                               .data$sent_match_tagged),
                        !!dplyr::quo_name(sent_match_tagged) := dplyr::if_else(is.na(.data$coll_pattern) & stringr::str_detect(.data$span, "^r"),
                                                                               stringr::str_replace(sent_match_tagged,
                                                                                                    paste("((?i)", nodes, ")(([^a-zA-Z0-9-]+[a-zA-Z0-9-[:punct:]]+){", as.numeric(stringr::str_replace(.data$span, "\\D", "")), "}[^a-zA-Z0-9-]+)((?i)", colloc, ")", sep = ""),
                                                                                                    "<n>\\1</n>\\2<c>\\4</c>"),
                                                                               .data$sent_match_tagged),
                        !!dplyr::quo_name(sent_match_tagged) := dplyr::if_else(is.na(.data$coll_pattern) & stringr::str_detect(.data$span, "^r") & stringr::str_detect(.data$span, "1$"),
                                                                               stringr::str_replace(sent_match_tagged,
                                                                                                    paste("((?i)", nodes, ")(([^a-zA-Z0-9-]+[a-zA-Z0-9-[:punct:]]+){", as.numeric(stringr::str_replace(.data$span, "\\D", "")), "}[^a-zA-Z0-9-]+)((?i)", colloc, ")", sep = ""),
                                                                                                    "<n>\\1</n>\\2<c>\\3</c>"),
                                                                               .data$sent_match_tagged)
    )

    df <- dplyr::mutate(df,
                        !!dplyr::quo_name(coll_pattern) := dplyr::if_else(is.na(.data$coll_pattern),
                                                                          stringr::str_extract(.data$sent_match_tagged, "<.+\\/.>"),
                                                                          .data$coll_pattern))

  }
  return(df[, -6])

}
