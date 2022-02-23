#' Sum of logical expressions
#' @name sum_and3s
#' @aliases sum_or3s
#' @param exprA,exprB,exprC,... Expressions of the form \code{x <op> y}.
#' with \code{<op>} one of the standard binary operators.
#' @param nThread \describe{
#' \item{\code{integer(1)}}{Number of threads to use.}
#' }
#' @param .env The environment in which the expressions are to be evaluated.
#'
#' @return
#' Equivalent to \code{sum(exprA & exprB & exprC)} or
#' \code{sum(exprA | exprB | exprC)} as desired.
#'
#' @export sum_and3s


sum_and3s <- function(exprA, exprB, exprC, ...,
                      nThread = getOption("hutilscpp.nThread", 1L),
                      .env = parent.frame()) {
  if (missing(exprB) && missing(exprC)) {
    return(sum_raw(eval.parent(substitute(and3s(exprA, nThread = nThread, type = "raw"))), nThread = nThread))
  }
  if (missing(exprC)) {
    return(sum_raw(eval.parent(substitute(and3s(exprA, exprB, nThread = nThread, type = "raw"))), nThread = nThread))
  }
  if (missing(exprB)) {
    return(sum_raw(eval.parent(substitute(and3s(exprA, exprC, nThread = nThread, type = "raw"))), nThread = nThread))
  }
  sum_raw(eval.parent(substitute(and3s(exprA, exprB, exprC, ..., nThread = nThread, type = "raw"))), nThread = nThread)
}

#' @rdname sum_and3s
#' @export

sum_or3s <- function(exprA, exprB, exprC, ...,
                     .env = parent.frame(),
                     nThread = getOption("hutilscpp.nThread", 1L)) {
  sum_raw(eval.parent(substitute(or3s(exprA, exprB, exprC, ..., nThread = nThread, type = "raw"))), nThread = nThread)
}

