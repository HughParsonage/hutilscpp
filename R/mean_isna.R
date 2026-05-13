#' Proportion of missing values
#' @description The proportion of missing values in an atomic vector,
#' equivalent to \code{mean(is.na(x))}.
#'
#' Introduced in \pkg{hutilscpp} 0.11.0; require at least that version
#' if your code depends on this function.
#' @inheritParams sum_isna
#'
#' @examples
#' mean_isna(c(1:5, NA))
#' @export

mean_isna <- function(x, do_anyNA = TRUE, nThread = getOption("hutilscpp.nThread", 1L)) {
  sum_isna(x, do_anyNA = do_anyNA, nThread = nThread) / length(x)
}
