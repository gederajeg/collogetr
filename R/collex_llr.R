#' Log-likelihood ratio
#'
#' @description Perform log-likelihood ratio as the association/collostruction strength.
#' @param df The output of \code{\link{assoc_prepare}}.
#' @param collstr_digit The numeric vector for floating digits of the collostruction strength. The default is \code{3}.
#'
#' @return A tibble consisting of the collocates (column \code{w}),
#'     co-occurrence frequencies with the node (column \code{a}),
#'     the expected co-occurrence frequencies with the node (column \code{a_exp}),
#'     the direction of the association (e.g., \emph{attraction} or \emph{repulsion}) (column \code{assoc}),
#'     the log-likelihood ratio as the association/collostruction strength measure (column \code{llr}).
#'     and two uni-directional association measures of \emph{Delta P}.
#' @export
#' @importFrom rlang syms
#' @importFrom dplyr mutate_if
#'
#' @examples
#' out <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
#'                       pattern = "mengatakan",
#'                       window = "r",
#'                       span = 3L,
#'                       save_interim = FALSE)
#' assoc_tb <- assoc_prepare(colloc_out = out, stopword_list = stopwords)
#' am_llr <- collex_llr(df = assoc_tb, collstr_digit = 3)
collex_llr <- function(df = NULL, collstr_digit = 3) {

  column_names <- colnames(df)
  group_for_nest <- column_names[column_names != "data"]
  group_for_nest <- rlang::syms(group_for_nest)

  a <- dplyr::quo(a)
  b <- dplyr::quo(b)
  c <- dplyr::quo(c)
  d <- dplyr::quo(d)
  b_exp <- dplyr::quo(b_exp)
  c_exp <- dplyr::quo(c_exp)
  d_exp <- dplyr::quo(d_exp)
  n_pattern <- dplyr::quo(n_pattern)
  n_w_in_corp <- dplyr::quo(n_w_in_corp)
  corpus_size <- dplyr::quo(corpus_size)
  w <- dplyr::quo(w)
  node <- dplyr::quo(node)

  df <- tidyr::unnest(df, .data$data)

  # change the integers into double to prevent error of floating integer when calculating the expected frequency
  df <- dplyr::mutate_if(dplyr::ungroup(df), is.integer, as.double)

  # get the expected frequency of the cells
  df <- dplyr::mutate(df,
                      !!dplyr::quo_name(b_exp) := round(((!!b + !!d) * !!n_w_in_corp)/!!corpus_size, collstr_digit),
                      !!dplyr::quo_name(c_exp) := round((!!n_pattern * (!!c + !!d))/!!corpus_size, collstr_digit),
                      !!dplyr::quo_name(d_exp) := round(((!!b + !!d) * (!!c + !!d))/!!corpus_size, collstr_digit))

  # nest again the relevant data
  df <- tidyr::nest(dplyr::group_by(df, !!!group_for_nest),
                    data = -c(!!w, !!node))

  log_likelihood_ratio <- function(df_nested) {

    # adapted from Gries' Coll.Ana for tidy operation by row
    s1 <- dplyr::if_else(log((df_nested$a/df_nested$a_exp), base = exp(1)) * df_nested$a == "NaN", 0, log((df_nested$a/df_nested$a_exp), base = exp(1)) * df_nested$a)
    s2 <- dplyr::if_else(log((df_nested$b/df_nested$b_exp), base = exp(1)) * df_nested$b == "NaN", 0, log((df_nested$b/df_nested$b_exp), base = exp(1)) * df_nested$b)
    s3 <- dplyr::if_else(log((df_nested$c/df_nested$c_exp), base = exp(1)) * df_nested$c == "NaN", 0, log((df_nested$c/df_nested$c_exp), base = exp(1)) * df_nested$c)
    s4 <- dplyr::if_else(log((df_nested$d/df_nested$d_exp), base = exp(1)) * df_nested$d == "NaN", 0, log((df_nested$d/df_nested$d_exp), base = exp(1)) * df_nested$d)
    llr <- 2 * sum(s1, s2, s3, s4)
    return(llr)
  }

  llr <- dplyr::quo(llr)
  dP_collex_cue_cxn <- dplyr::quo(dP_collex_cue_cxn)
  dP_cxn_cue_collex <- dplyr::quo(dP_cxn_cue_collex)

  df <- dplyr::mutate(df,
                      !!dplyr::quo_name(llr) := purrr::map_dbl(data, log_likelihood_ratio),
                      !!dplyr::quo_name(dP_collex_cue_cxn) := purrr::map_dbl(data, dP_cue_cxn, collstr_digit = collstr_digit),
                      !!dplyr::quo_name(dP_cxn_cue_collex) := purrr::map_dbl(data, dP_cue_collex, collstr_digit = collstr_digit))
  df <- tidyr::unnest(df, .data$data)
  df <- dplyr::mutate(df,
                      !!dplyr::quo_name(llr) := dplyr::if_else(.data$a > .data$a_exp,
                                                               round(llr, collstr_digit),
                                                               round(-llr, collstr_digit)))

  df <- df[, -grep("^((b|c|d)(_exp)?|n_w_in_corp|corpus_size|n_pattern)$", colnames(df), perl = TRUE)]
  df <- dplyr::select(df, .data$w, .data$node, .data$a, .data$a_exp, .data$assoc, .data$llr, .data$dP_collex_cue_cxn, .data$dP_cxn_cue_collex, dplyr::everything())
  return(df)
}
