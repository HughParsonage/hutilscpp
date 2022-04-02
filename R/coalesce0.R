#' Convenience function for coalescing to zero
#' @param x An atomic vector.
#' @param nThread Number of threads to use.
#'
#' @return Equivalent to \code{hutils::coalesce(x, 0)} for
#' an appropriate type of zero.
#'
#' @export


coalesce0 <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
  .Call("Ccoalesce0", x, nThread, PACKAGE = "hutilscpp")
}

uncoalesce0 <- function(x) {
  .Call("Cuncoalesce0", x, PACKAGE = "hutilscpp")
}
