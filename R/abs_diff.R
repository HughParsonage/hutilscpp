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
#' \item{3}{Return \code{which.max(abs(x - y))}}
#' }
#'
#' @examples
#' x <- sample(10)
#' y <- sample(10)
#' abs_diff(x, y)
#' max_abs_diff(x, y)
#'
#' @export
abs_diff <- function(x, y, nThread = getOption("hutilscpp.nThread", 1L), option = 1L) {
  if (is.double(y) && length(y) == 1L) {
    y <- as_integer_if_safe(y)
  }
  if (option == 3L) {
    return(.Call("C_which_abs_diff", x, y, nThread, PACKAGE = "hutilscpp"))
  }
  ans <- .Call("C_abs_diff", x, y, nThread, option, PACKAGE = "hutilscpp")
  # nocov start
  if (is.null(ans)) {
    if (option == 0L) {
      return(max(abs(x - y)))
    }
    return(abs(x - y))
  }
  # nocov end
  ans
}

#' @rdname abs_diff
#' @export
max_abs_diff <- function(x, y, nThread = getOption("hutilscpp.nThread", 1L)) {
  abs_diff(x, y, nThread, option = 0L)
}

