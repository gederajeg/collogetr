#' Fisher's Exact Test for DCA
#'
#' @description Perform one-tailed Fisher's Exact test for the \emph{Distinctive Collexeme/Collocates Analysis}.
#' @param df The output of \code{\link{assoc_prepare_dca}}.
#' @param collstr_digit The numeric vector for floating digits of the collostruction strength. The default is \code{3}.
#' @return A data frame containing the collocates, their frequency with Construction/Word A and B, p-value of the Fisher Test, Collostruction Strength, and information on the distinctiveness of the collocates with the constructions.
#' @export
#' @examples
#' \dontrun{
#'
#' # get the input format via assoc_prepare_dca()
#' dca_tb <- assoc_prepare_dca(assoc_tb)
#'
#' # perform the DCA
#' dca_res <- collex_fye_dca(dca_table)
#'
#' }
collex_fye_dca <- function(df = NULL, collstr_digit = 3) {
  p_fye <- dplyr::quo(p_fye)
  w <- dplyr::quo(w)
  columns <- colnames(df)[2:3]
  colnames(df)[2:3] <- c("a", "b")
  df <- tidyr::nest(dplyr::group_by(df, !!w),
                    data = -!!w)

  df <- dplyr::mutate(df,
                      !!dplyr::quo_name(p_fye) := purrr::map_dbl(data, fye_compute))
  df <- tidyr::unnest(df, .data$data)
  df$dist_for <- dplyr::if_else(df$a > df$a_exp, columns[1], columns[2])
  df$collstr <- dplyr::if_else(df$a > df$a_exp,
                               round(-log10(df$p_fye), collstr_digit),
                               round(-log10(df$p_fye), collstr_digit))
  df <- dplyr::select(df, .data$w, .data$a, .data$b, .data$p_fye, .data$collstr, .data$dist_for)
  colnames(df)[2:3] <- columns
  df <- dplyr::arrange(df, .data$dist_for, desc(.data$collstr))
  return(df)
}

#' Retrieve the distinctive collocates/collexemes of construction/word X
#'
#' @description A helper function to extract distinctive collocates/collexemes for construction/word X from the output of \code{\link{collex_fye_dca}}.
#' @param dca_res The output of \code{\link{collex_fye_dca}}
#' @param dist_for Character vectors of names of the contrasted constructions, or either \code{"a"}/\code{"A"}, or \code{"b"}/\code{"B"}. This argument indicate the preference of the collexeme/collocates (i.e. whether to Construction A or B).
#' \code{Construction A} is the name for the \emph{second} column from the output of \code{\link{collex_fye_dca}}. \code{Construction B} is the name for the \emph{third} column from the output of \code{\link{collex_fye_dca}}.
#' @param top_n Integer for the top-n distinctive collocates/collexemes. The default is \code{20L}.
#'
#' @return A data frame of distinctive collocates/collexemes for construction X.
#' @export
#'
#' @examples
#' \dontrun{
#' # distinctive for CxN B
#' dca_top_collex(dca_res, "b")
#'
#' # distinctive for CxN A
#' dca_top_collex(dca_res, "a")
#' }
dca_top_collex <- function(dca_res, dist_for = NULL, top_n = 20L) {
  if (dist_for %in% c("a", "A", colnames(dca_res)[2])) {
    cxn_a <- colnames(dca_res)[2]
    dist_df <- dplyr::filter(dca_res, dist_for == cxn_a)
    # dist_df <- dplyr::select(dist_df, -.data$a_exp)
  } else if (dist_for %in% c("b", "B", colnames(dca_res)[3])) {
    cxn_b <- colnames(dca_res)[3]
    dist_df <- dplyr::filter(dca_res, dist_for == cxn_b)
    # dist_df <- dplyr::select(dist_df, -.data$a_exp)

  }
  if (!is.null(top_n)) {
    dist_df <- dplyr::top_n(dist_df, n = top_n, wt = .data$collstr)
    return(dist_df)
  } else {
    return(dist_df)
  }
}
