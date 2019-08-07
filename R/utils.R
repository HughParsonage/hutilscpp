check_TF <- function(x) {
  if (is.logical(x) && length(x) == 1L) {
    if (anyNA(x)) {
      xc <- deparse(substitute(x))
      stop("`", xc, " = NA` but must be TRUE or FALSE. ",
           "Change `", xc, "` to be TRUE or FALSE.")
    } else {
      return(NULL)
    }
  } else {
    xc <- deparse(substitute(x))
    if (length(x) != 1L) {
      stop("`", xc, "` had length ", length(x), " but must be length-one. ",
           "Change `", xc, "` to be TRUE or FALSE.")
    } else {
      stop("`", xc, "` was type ", typeof(x), " but must be logical. ",
           "Change `", xc, "` to be TRUE or FALSE.")
    }
  }
}

isnt_number <- function(a, na.bad = TRUE, infinite.bad = TRUE) {
  if (!is.numeric(a)) {
    o <- TRUE
    ac <- deparse(substitute(a))
    attr(o, "ErrorMessage") <- paste0("`", ac, "` was a ", class(a), ", but must be numeric.")
    return(o)
  }
  if (length(a) != 1L) {
    o <- TRUE
    ac <- deparse(substitute(a))
    attr(o, "ErrorMessage") <- paste0("`", ac, "` had length ", length(a), ", but must be length-one.")
    return(o)
  }
  if (na.bad && is.na(a)) {
    o <- TRUE
    ac <- deparse(substitute(a))
    attr(o, "ErrorMessage") <- paste0("`", ac, "= NA`, but this is not permitted.")
    return(o)
  }
  if (infinite.bad && is.infinite(a)) {
    o <- TRUE
    ac <- deparse(substitute(a))
    attr(o, "ErrorMessage") <- paste0("`", ac, "` was not finite, but this is not permitted.")
    return(o)
  }
  FALSE
}

AND <- `&&`
OR <- `||`

epsilon <- function() {
  sqrt(.Machine$double.eps)
}

#' @noRd
#' @param xi integer version of \code{x}. May be cheaper if already known
which_isnt_integerish <- function(x, xi = as.integer(x)) {
  if (is.integer(x)) {
    return(0L)
  }
  e <- epsilon()
  # slower to use -e, e when *validating* data,
  # which should be the benchmark, since it
  # doesn't matter how fast you are when you
  # are about to error.
  d_r <- do_range_dbl(x - xi)


  if (d_r[2L] > e) {
    return(as.integer(d_r[4L]))
  }
  if (d_r[1L] < -e) {
    return(as.integer(d_r[3L]))
  }
  0L
}

isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !anyNA(x) && !x
}


firstNonNegativeRadix <- function(x, ...) {
  if (is.double(x)) {
    do_firstNonNegativeRadix_dbl(x, ...)
  } else {
    do_firstNonNegativeRadix_int(x, ...)
  }
}
