#' Is a vector empty?
#' @description A vector is empty if \code{all(is.na(x))} with a
#' special case for \code{length(x) == 0}.
#'
#' @param x A vector. Only atomic vectors are supported.
#' @param expected \code{TRUE | FALSE} Whether it is expected that \code{x} is empty.
#' If \code{TRUE} the function will be marginally faster if \code{x} is empty but
#' likely slower if not.
#' @param len0 The result if \code{length(x) == 0}.
#' @param nThread Number of threads to use (only applicable if \code{expected} is \code{TRUE})
#'
#' @examples
#' allNA(c(NA, NA))
#' allNA(c(NA, NA, 1))
#'
#' @export
allNA <- function(x, expected = FALSE, len0 = FALSE, nThread = getOption("hutilscpp.nThread", 1L)) {
  .Call("C_empty", x, expected, len0, nThread, PACKAGE = "hutilscpp")
}

