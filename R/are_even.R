#' Are even
#' @param x An integer vector. Double vectors may also be used.
#' @param check_integerish (logical, default: \code{TRUE}) Should the
#' values in \code{x} be checked for non-integer values
#' if \code{x} is a double vector. If \code{TRUE}
#' and values are found to be non-integer a warning is emitted.
#'
#'
#'
#' @return For \code{are_even}, a logical vector the same length as \code{x},
#' \code{TRUE} whenever \code{x} is even.
#'
#' For \code{which_are_even} the integer positions of even values in \code{x}.
#'
#'
#'
#'
#'
#' @export are_even which_are_even

are_even <- function(x, check_integerish = TRUE) {
  if (is.integer(x)) {
    return(do_are_even(x, double(0)))
  }
  if (is.double(x)) {
    if (AND(check_integerish,
            wb <- which_isnt_integerish(x))) {
      warning("`x` was type double, but element ",
              wb, " = ", x[wb], " was not an integer value. ",
              "Will be coerced to integer: ", as.integer(x[wb]), ".")
    }
    do_are_even(integer(0), x)
  } else {
    stop("`x` was not an integer or double.")
  }
}

#' @rdname are_even
which_are_even <- function(x, check_integerish = TRUE) {
  if (length(x) >= .Machine$integer.max) {
    stop("`which_are_even() not implemented for long `x`.")
  }
  if (is.integer(x)) {
    return(do_which_even(x, double(0)))
  }
  if (is.double(x)) {
    if (AND(check_integerish,
            wb <- which_isnt_integerish(x))) {
      warning("`x` was type double, but element ",
              wb, " = ", x[wb], " was not an integer value. ",
              "Will be coerced to integer: ", as.integer(x[wb]), ".")
    }
    return(do_which_even(integer(0), x))
  } else {
    stop("`x` was not an integer or double.")
  }
}

