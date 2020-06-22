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

#' @noRd
#' @param x A vector, likely to be double.
#' @param xi integer version of \code{x}. May be cheaper if already known
#' @param anyNAs Does `x` contain any NA or NaN values?
which_isnt_integerish <- function(x, xi = as.integer(x), check_finite = TRUE) {
  if (is.integer(x)) {
    return(0L)
  }
  if (!is.double(x)) {
    return(1L)
  }
  if (!isFALSE(check_finite) && {nfx <- do_anyNonfinite(x)}) {
    return(nfx)
  }

  storage.mode(xi) <- "double"
  which_first(x != xi)
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

g <- glue::glue

