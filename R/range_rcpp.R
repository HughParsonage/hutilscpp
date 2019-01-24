#' Range C++
#' @param x A vector for which the range is desired. Missing values
#' have no defined behaviour.
#' @param warn_empty (logical, default: \code{TRUE}) If \code{x} is
#' empty (i.e. has no length), should a warning be emitted (like \code{\link[base]{range}})?
#'
#' @param integer0_range_is_integer (logical, default: \code{FALSE})
#' If \code{x} is a length-zero integer, should the result also be an integer?
#' Set to \code{FALSE} for compatibility with \code{\link[base]{range}}, but can
#' be set to \code{TRUE} if an integer result is desired, in which case
#' \code{range_rcpp(integer())} is \code{(INT_MAX, -INT_MAX)}.
#'
#' @return A vector, the first two positions give the range and
#' the next two give the positions where the max and min occur.
#'
#'
#' @export range_rcpp

range_rcpp <- function(x, warn_empty = TRUE, integer0_range_is_integer = FALSE) {
  if (!length(x)) {
    if (is.integer(x) && integer0_range_is_integer) {
      if (warn_empty) {
        warning("no non-missing arguments to range_rcpp; returning c(INT_MAX, -INT_MAX).")
      }
      out <- c(.Machine$integer.max, -.Machine$integer.max)
    } else {
      if (warn_empty) {
        warning("no non-missing arguments to range_rcpp; returning c(Inf, -Inf).")
      }
      out <- c(Inf, -Inf)
    }
    return(out)
  }
  if (is.integer(x)) {
    return(do_range_int(x))
  }
  if (is.double(x)) {
    return(do_range_dbl(x))
  }
  range(x)
}


