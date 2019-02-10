#' Is a vector constant?
#' @description Efficiently decide whether an atomic vector is constant; that is,
#' contains only one value.
#'
#' Equivalent to
#'
#' \code{data.table::uniqueN(x) == 1L}
#'
#' or
#'
#' \code{forecast::is.constant(x)}
#'
#' @param x An atomic vector.
#'
#' @return \code{TRUE} or \code{FALSE}. Missing values are considered to
#' be the same.
#'
#'
#' @export

is_constant <- function(x) {
  if (length(x) <= 1L) {
    return(TRUE)
  }
  if (is.logical(x)) {
    return(!any(x, na.rm = TRUE) && all(x, na.rm = TRUE))
  }
  x1 <- x[1L]
  if (anyNA(x1)) {
    return(all(is.na(x)))
  }

  if (is.integer(x)) {
    !AnyWhich_int(x, x1, FALSE, FALSE, FALSE)
  } else if (is.double(x)) {
    !AnyWhich_dbl(x, x1, FALSE, FALSE, FALSE)
  } else if (is.character(x)) {
    # Does any character in x not not match the first?
    # (double negative because the single positive only
    # checks the second element)
    !AnyCharMatch(x, x1, opposite = TRUE)
  } else if (is.factor(x)) {
    return(is_constant(as.integer(x)))
  } else {
    # e.g. raw
    identical(x, rep_len(x1, length(x)))
  }
}

zz3 <- function(x) {
  as.logical(which.max(x != x[1L]))
}



