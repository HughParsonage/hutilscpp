#' Divisibility
#'
#' @param x An integer vector
#' @param d \code{integer(1)}. The divisor.
#' @param nThread The number of threads to use.
#'
#' @return Logical vector: \code{TRUE} where \code{x} is divisible by \code{d}.
#'
#' \code{divisible2},\code{divisible16} are short for (and quicker than)
#' \code{divisible(x, 2)} and \code{divisble(x, 16)}.
#'
#' @export

divisible <- function(x, d, nThread = getOption("hutilscpp.nThread", 1L)) {
  nThread <- ensure_integer(nThread)
  .Call("Cdivisible", x, d, nThread, PACKAGE = packageName)
}

#' @rdname divisible
#' @export
divisible2 <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
  nThread <- ensure_integer(nThread)
  .Call("Cdivisible2", x, nThread, TRUE, PACKAGE = packageName)
}

#' @rdname divisible
#' @export
divisible16 <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
  .Call("Cdivisible16", x, nThread, PACKAGE = packageName)
}


