#' @title Parallel maximum/minimum
#' @description Faster \code{pmax()} and \code{pmin()}.
#'
#' @name pmaxC
#' @param x A numeric vector.
#' @param y,z Other numeric vectors the same length as \code{x}
#' @param a A single numeric value.
#'
#' @param in_place (logical, default: \code{FALSE}) Should \code{x}
#' be modified in-place.
#' @param sorted If \code{TRUE}, \code{x} is assumed to be sorted. Thus the
#'  first zero determines whether the position at which zeroes start or end.
#' @return The parallel maximum/minimum of the input values. \code{pmax0(x)} is
#'  shorthand for \code{pmaxC(x, 0)}, i.e. convert negative values in \code{x} to 0.
#' @note This function will always be faster than \code{pmax(x, a)} when \code{a} is
#'  a single value, but can be slower than \code{pmax.int(x, a)} when \code{x} is short.
#'  Use this function when comparing a numeric vector with a single value.
#'
#'  Use \code{in_place = TRUE} only within functions when you are sure it is safe, i.e. not a
#'  reference to something outside the environment.
#'
#'  If \code{x} is nonnegative so \code{pmax0(x) = identity(x)} the function will be much faster
#'  still, as the \code{C++} code only starts allocating once a negative value is found.
#'
#' @examples
#' pmaxC(-5:5, 2)
#'
#' @export pmaxC pmax0 pmaxV pmax3
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
  if (is.double(x) && is.integer(a)) {
    return(do_pmaxC_dbl(x, as.double(a), in_place = in_place))
  }

  stop("Internal error pmaxC:65") # nocov
}

#' @rdname pmaxC
pmax0 <- function(x, in_place = FALSE, sorted = FALSE) {
  if (!is.atomic(x) || !is.numeric(x)) {
    stop("`x` was a ", class(x), ", but must be numeric.")
  }
  if (!length(x)) {
    return(x)
  }
  if (isTRUE(sorted)) {
    if (is.integer(x)) {
      do_pmax0_radix_sorted_int(x, in_place = TRUE)
    } else {
      do_pmax0_radix_sorted_dbl(x, in_place = TRUE)
    }
  }

  if (in_place) {
    if (is.integer(x)) {
      do_pmaxIPint0(x)
    } else {
      do_pmaxIPnum0(x)
    }
  } else {
    if (is.integer(x)) {
      do_pmax0_abs_int(x)
    } else {
      do_pmax0_abs_dbl(x)
    }
  }
}

#' @rdname pmaxC
pmaxV <- function(x, y, in_place = FALSE) {
  if (is.integer(x) && is.integer(y)) {
    return(do_pmaxIntInt(x, y))
  }
  if (is.double(x) && is.double(y)) {
    return(do_pmaxNumNum(x, y))
  }
  if (is.integer(y)) {
    stop("`y` is type integer, yet `x` is type double.")
  }
  if (is.double(y)) {
    stop("`y` is type double, yet `x` is type integer.")
  }
}

#' @rdname pmaxC
pmax3 <- function(x, y, z, in_place = FALSE) {
  check_TF(in_place)
  lx <- length(x)
  if (length(y) == lx && length(z) == lx) {
    if (is.integer(x) && is.integer(y) && is.integer(z)) {
      return(do_summary3_int(x, y, z, in_place, do_max = TRUE))
    }
    if (is.double(x) && is.double(y) && is.double(z)) {
      return(do_summary3_dbl(x, y, z, in_place, do_max = TRUE))
    }
  }
  if (!is.numeric(x) || !is.numeric(y) || !is.numeric(z)) {
    stop("`x` was of type ", typeof(x),
         "`y` was of type ", typeof(y),
         "`z` was of type ", typeof(z), ". ",
         "All of `x`, `y`, and `z` must be numeric.")
  }
  # lengths differ
  if (length(y) != lx && length(y) != 1L) {
    stop("`y` had length ", length(y), ", yet ",
         "`x` had length ", length(x), ". ",
         "`y` and `z` must be the same length as `x`, (or length-one).")
  }
  if (length(z) != lx && length(z) != 1L) {
    stop("`z` had length ", length(z), ", yet ",
         "`x` had length ", length(x), ". ",
         "`y` and `z` must be the same length as `x`, (or length-one).")
  }

  if (is.integer(x) && (is.double(y) || is.double(z))) {
    yi <- y
    zi <- z
    if (is.double(y)) {
      yi <- as.integer(y)
      if (AND(is.double(y),
              wb <- which_isnt_integerish(y, yi))) {
        stop("`x` was type integer and `y` was type double, but entry ", wb,
             " was not equal to the integer equivalent. ")
      }
    }
    if (is.double(z)) {
      zi <- as.integer(z)
      if (AND(is.double(z),
              wb <- which_isnt_integerish(z, zi))) {
        stop("`x` was type integer and `z` was type double, but entry ", wb,
             " was not equal to the integer equivalent. ")
      }
    }
    return(do_summary3_int(x, yi, zi, in_place = in_place, do_max = TRUE))
  }
  if (is.double(x) && is.numeric(y) && is.numeric(z)) {
    return(do_summary3_dbl(x, as.double(y), as.double(z), in_place = in_place, do_max = TRUE))
  }

  pmax(x, pmax(y, z)) # nocov
}


