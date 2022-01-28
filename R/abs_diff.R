#' Absolute difference
#' @description Equivalent to \code{abs(x - y)} but aims to be faster by
#' avoiding allocations.
#' @param x,y Atomic, numeric, equilength vectors.
#' @param nThread Number of threads to use.
#' @param option An integer, provides backwards-compatible method to change results.
#' \describe{
#' \item{0}{Return \code{max(abs(x - y))} (without allocation).}
#' \item{1}{Return \code{abs(x - y)} with the expectation that every element will be \code{integer},
#' returning a \code{double} only if required.}
#' \item{2}{Return \code{abs(x - y)} but always a \code{double} vector, regardless of necessity.}
#' }
#'
#' @export
abs_diff <- function(x, y, nThread = getOption("hutilscpp.nThread", 1L), option = 1L) {
  ans <- .Call("C_abs_diff", x, y, nThread, option, PACKAGE = "hutilscpp")
  if (is.null(ans)) {
    return(abs(x - y))
  }
  ans
}

