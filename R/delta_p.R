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
