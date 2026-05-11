#' Sum of logical expressions
#' @name sum_and3s
#' @aliases sum_or3s
#' @param exprA,exprB,exprC,... Expressions of the form \code{x <op> y}.
#' with \code{<op>} one of the standard binary operators.
#' @param na See \code{\link{and3s}}. Default \code{"false"}: NA in the
#' mask is coerced to FALSE before summing. \code{"base"}: NA propagates
#' to the sum, matching \code{sum(exprA & exprB & ...)} (returns NA when
#' any predicate value is NA).
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
                      na = c("false", "base"),
                      nThread = getOption("hutilscpp.nThread", 1L),
                      .env = parent.frame()) {
  na <- match.arg(na)
  # `na = "base"` propagates NA per base R semantics, so the sum must be
  # allowed to be NA -- request `type = "logical"` and use base `sum()`.
  # The default fast path stays on raw + sum_raw. Each branch must
  # include `...` so user-supplied options (unsupported / recycle / ...)
  # also propagate to and3s.
  if (na == "base") {
    if (missing(exprB) && missing(exprC)) {
      return(sum(eval.parent(substitute(and3s(exprA, ..., na = na, nThread = nThread, type = "logical")))))
    }
    if (missing(exprC)) {
      return(sum(eval.parent(substitute(and3s(exprA, exprB, ..., na = na, nThread = nThread, type = "logical")))))
    }
    if (missing(exprB)) {
      return(sum(eval.parent(substitute(and3s(exprA, exprC, ..., na = na, nThread = nThread, type = "logical")))))
    }
    return(sum(eval.parent(substitute(and3s(exprA, exprB, exprC, ..., na = na, nThread = nThread, type = "logical")))))
  }
  if (missing(exprB) && missing(exprC)) {
    return(sum_raw(eval.parent(substitute(and3s(exprA, ..., na = na, nThread = nThread, type = "raw"))), nThread = nThread))
  }
  if (missing(exprC)) {
    return(sum_raw(eval.parent(substitute(and3s(exprA, exprB, ..., na = na, nThread = nThread, type = "raw"))), nThread = nThread))
  }
  if (missing(exprB)) {
    return(sum_raw(eval.parent(substitute(and3s(exprA, exprC, ..., na = na, nThread = nThread, type = "raw"))), nThread = nThread))
  }
  sum_raw(eval.parent(substitute(and3s(exprA, exprB, exprC, ..., na = na, nThread = nThread, type = "raw"))), nThread = nThread)
}

#' @rdname sum_and3s
#' @export

sum_or3s <- function(exprA, exprB, exprC, ...,
                     na = c("false", "base"),
                     .env = parent.frame(),
                     nThread = getOption("hutilscpp.nThread", 1L)) {
  na <- match.arg(na)
  # See sum_and3s comment.
  if (na == "base") {
    return(sum(eval.parent(substitute(or3s(exprA, exprB, exprC, ..., na = na, nThread = nThread, type = "logical")))))
  }
  sum_raw(eval.parent(substitute(or3s(exprA, exprB, exprC, ..., na = na, nThread = nThread, type = "raw"))), nThread = nThread)
}
