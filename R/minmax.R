#' @title Minimum and maximum
#' @param x An atomic vector.
#' @param empty_result What should be returned when \code{length(x) == 0}?
#' @param nThread Number of threads to be used.
#'
#' @return Vector of two elements, the minimum and maximum of \code{x}, or \code{NULL}.
#'
#' @export
minmax <- function(x, empty_result = NULL, nThread = getOption("hutilscpp.nThread", 1L)) {
  .Call("Cminmax", x, empty_result, check_omp(nThread), PACKAGE = packageName)
}
