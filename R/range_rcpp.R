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
#' Essentially this is equivalent to \code{c(range(x), which.min(x), which.max(x))}.
#' Note that the type is not strictly preserved, but no loss should occur. In particular,
#' logical \code{x} results in an integer result, and a double \code{x} will
#' have double values for \code{which.min|max(x)}.
#'
#'
#' @export range_rcpp

range_rcpp <- function(x,
                       warn_empty = TRUE,
                       integer0_range_is_integer = FALSE) {
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
  if (is.logical(x)) {
    if (anyNA(x)) {
      return(c(NA, NA, NA, NA))
    }
    if (any(x, na.rm = TRUE)) {
      if (all(x, na.rm = TRUE)) {
        o <- c(TRUE, TRUE, rep(do_which_first(x), 2L))
      } else {
        o <- c(FALSE, TRUE, do_which_first(x), do_which_first_false(x))
      }
    } else {
      o <- c(FALSE, FALSE, rep(do_which_first_false(x), 2L))
    }
    return(o)
  }
  range(x)
}


