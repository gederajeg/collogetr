#' DCA input table
#'
#'
#' @description A function to generate input table for performing Distinctive Collexeme Analysis.
#' @param assoc_tb_df The nested tibble output from \code{\link{assoc_prepare}}
#' @return A tibble with columns for collocates, frequency of the collocates with Construction/Word A and B, other frequencies in the cross-tabulation (i.e. Cell C and D), and the Expected frequencies of the collocates with Construction/Word A as the reference item.
#' @importFrom tidyr unnest
#' @importFrom dplyr select
#' @importFrom dplyr quo
#' @importFrom dplyr quo_name
#' @importFrom dplyr mutate
#' @importFrom tidyr spread
#' @importFrom rlang sym
#' @export
#' @examples
#' \dontrun{
#' # run assoc_prepare() first from the output of colloc_leipzig()
#'  assoc_tb <- assoc_prepare(colloc_out = colloc_leipzig_output,
#'                            window_span = "r1",
#'                            per_corpus = FALSE,
#'                            stopword_list = NULL,
#'                            float_digits = 3L)
#'
#' # then run assoc_prepare_dca()
#' dca_tb <- assoc_prepare_dca(assoc_tb)
#' }

assoc_prepare_dca <- function(assoc_tb_df = NULL) {
  # data comes from assoc_tb nested table
  dca_table <- tidyr::unnest(assoc_tb_df)
  dca_table <- dplyr::select(dca_table, .data$w, .data$node, .data$a)
  dca_table <- tidyr::spread(dca_table, key = .data$node, value = .data$a, fill = 0L)
  cxn_names <- colnames(dca_table)[-1]
  cxn_a_sym <- rlang::sym(cxn_names[1])
  cxn_b_sym <- rlang::sym(cxn_names[2])
  sum_cxn_a <- sum(dca_table[[2]])
  sum_cxn_b <- sum(dca_table[[3]])
  a_exp <- dplyr::quo(a_exp)
  c <- dplyr::quo(c)
  d <- dplyr::quo(d)
  dca_table <- dplyr::mutate(dca_table,
                             !!dplyr::quo_name(c) := sum_cxn_a - !!cxn_a_sym,
                             !!dplyr::quo_name(d) := sum_cxn_b - !!cxn_b_sym,
                             !!dplyr::quo_name(a_exp) := (!!cxn_a_sym + !!cxn_b_sym) * (!!cxn_a_sym + !!c)/(!!cxn_a_sym + !!cxn_b_sym + !!c + !!d))
  return(dca_table)
}
