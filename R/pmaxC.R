#' @title Parallel maximum/minimum
#' @description Faster \code{pmax()} and \code{pmin()}.
#'
#' @name pmaxC
#' @param x A numeric vector.
#' @param a A single numeric value.
#' @param in_place (logical, default: \code{FALSE}) Should \code{x}
#' be modified in-place.
#' @return The parallel maximum/minimum of the input values. \code{pmax0(x)} is
#'  shorthand for \code{pmaxC(x, 0)}, i.e. convert negative values in \code{x} to 0.
#' @note This function will always be faster than \code{pmax(x, a)} when \code{a} is
#'  a single value, but can be slower than \code{pmax.int(x, a)} when \code{x} is short.
#'  Use this function when comparing a numeric vector with a single value.
#'
#'  Use \code{in_place = TRUE} within functions when you are sure it is safe.
#'
#' @examples
#' pmaxC(-5:5, 2)
#'
#' @export pmaxC pmax0
#'
#'

pmaxC <- function(x, a, in_place = FALSE) {
  # The whole point of this function is speed,
  # so give results immediately if the user
  # provides results that are expected and safe.
  if (length(x) && length(a) == 1L) {
    if (is.integer(x) && is.integer(a)) {
      if (in_place) {
        return(do_pmaxIP_int(x, a))
      } else {
        return(do_pmaxC_int(x, a))
      }
    }
    if (is.double(x) && is.double(a)) {
      if (in_place) {
        return(do_pmaxIP_dbl(x, a))
      } else {
        return(do_pmaxC_dbl(x, a))
      }
    }
  }


  if (!is.numeric(x)) {
    stop("`x` was a ", class(x), ", but must be numeric.")
  }
  if (!length(x)) {
    return(x)
  }
  if (!is.numeric(a)) {
    stop("`a` was a ", class(a), ", but must be numeric.")
  }
  if (length(a) != 1L) {
    stop("`a` had length ", length(a), ", but must be length-one. ",
         "If you require the parallel maximum of two equal-length vectors, ",
         "use pmaxV(x, y).")
  }

  # e.g.
  # pmaxC(<int>, 0) => 0L
  # but we need to be careful
  # pmaxC(<int>, 0.5) !=> 0
  if (is.integer(x)) {
    # a must be double by now
    if (!is.double(a)) stop("Internal error pmaxC:55")  # nocov
    ai <- as.integer(a)
    OR <- `||`
    if (OR(ai == a,
           abs(ai - a) < sqrt(.Machine$double.eps))) {
      return(do_pmaxC_int(x, ai))
    } else {
      message("Output is double.")
      return(do_pmaxC_dbl(as.double(x), a))
    }
  }
  stop("Internal error pmaxC:65") # nocov
}

#' @rdname pmaxC
pmax0 <- function(x, in_place = FALSE) {
  if (!is.numeric(x)) {
    stop("`x` was a ", class(x), ", but must be numeric.")
  }
  if (!length(x)) {
    return(x)
  }
  if (in_place) {
    if (is.integer(x)) {
      do_pmaxIPint0(x)
    } else {
      do_pmaxIPnum0(x)
    }
  } else {
    if (is.integer(x)) {
      do_pmaxC_int(x, 0L)
    } else {
      do_pmaxC_dbl(x, 0)
    }
  }

}

#' @rdname pmaxC
#' @export pminC
pminC <- function(x, a = 0, in_place = FALSE) {
  if (!is.numeric(x)) {
    stop("`x` was a ", class(x), ", but must be numeric.")
  }
  if (!length(x)) {
    return(x)
  }
  if (!is.numeric(a)) {
    stop("`a` was a ", class(a), ", but must be numeric.")
  }
  if (length(a) != 1L) {
    stop("`a` had length ", length(a), ", but must be length-one. ",
         "If you require the parallel maximum of two equal-length vectors, ",
         "use pmaxV(x, y).")
  }
  check_TF(in_place)
  do_pminC(x, a, in_place = in_place)
}


