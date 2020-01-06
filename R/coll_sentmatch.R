#' Sentence match retriever
#'
#' @description The function extract full sentence-match for a (set of significant) collocate(s) for a given nodeword. Given that
#' @param collout List output of \code{\link{colloc_leipzig}}.
#' @param colloc Character vector of the collocate(s) whose sentence match(es) to be retrieved.
#' @param wspan Character vector of the window span in which the collocates occur. Default to \code{NULL}, which will retrieve the collocate's occurrence in all span.
#' @param nodeword Character vector specifying one of the nodewords if search parameter in \code{\link{colloc_leipzig}} includes more than one nodeword.
#' @param sampled Integer vector indicating the number of random sample of the sentence match to be retrieve. Default to \code{NULL}, which will retrieve all sentence-matches.
#'
#' @return Character vector of sentence-match(es).
#' @export
#'
#' @importFrom assertthat assert_that
#'
#' @examples
#' collout <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
#'                      pattern = "mengatakan",
#'                      window = "r",
#'                      span = 3,
#'                      save_interim = FALSE)
#'
#' colloc_sentmatch(collout,
#'                  colloc = "bahwa",
#'                  sampled = 10)
#'
#' # This will produce message indicating that
#' # the queried sample number is higher than
#' # the sentence match for "akan"
#' colloc_sentmatch(collout,
#'                  colloc = "akan",
#'                  sampled = 10)
colloc_sentmatch <- function(collout, colloc = NULL, wspan = NULL, nodeword = NULL, sampled = NULL) {

  assertthat::assert_that(!is.null(colloc),
                          msg = "`colloc` argument cannot be NULL. Specify with one or more collocates!")

  if(is.null(nodeword)) {

    if(!is.null(wspan)) {

      sent <- subset(collout[[1]], w %in% colloc & span %in% wspan)$sent_match

    } else {

      sent <- subset(collout[[1]], w %in% colloc)$sent_match

    }

  } else if (!is.null(nodeword)) {

    if(!is.null(wspan)) {

      sent <- subset(collout[[1]], w %in% colloc & node %in% nodeword & span %in% wspan)$sent_match

    } else {

      sent <- subset(collout[[1]], w %in% colloc & node %in% nodeword)$sent_match

    }

  }

  if(!is.null(sampled)) {

    if(length(sent) > sampled) {

      return(sample(sent, sampled, TRUE))

    } else {

      message(cat(paste("Returning all matches!\nLength of matches (",
                        length(sent),
                        ") is lower than the number of the queried sample (",
                        sampled,
                        ").\n",
                        sep = "")))
      return(sent)

    }

  } else if(is.null(sampled)) {

    return(sent)

  }

}
