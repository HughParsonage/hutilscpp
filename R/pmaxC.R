#' @title Parallel maximum/minimum
#' @description Faster \code{pmax()} and \code{pmin()}.
#'
#' @name pmaxC
#' @param x A numeric vector.
#' @param a A single numeric value.
#' @return The parallel maximum/minimum of the input values. \code{pmax0(x)} is
#'  shorthand for \code{pmaxC(x, 0)}, i.e. convert negative values in \code{x} to 0.
#' @note This function will always be faster than \code{pmax(x, a)} when \code{a} is
#'  a single value, but can be slower than \code{pmax.int(x, a)} when \code{x} is short.
#'  Use this function when comparing a numeric vector with a single value.
#'
#' @examples
#' pmaxC(-5:5, 2)
#'
#' @export pmaxC pmax0
#'
#'

pmaxC <- function(x, a) {
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
  do_pmaxC(x, a)
}

#' @rdname pmaxC
pmax0 <- function(x) {
  if (!is.numeric(x)) {
    stop("`x` was a ", class(x), ", but must be numeric.")
  }
  if (!length(x)) {
    return(x)
  }
  if (is.integer(x)) {
    do_pmaxIPint0(x)
  } else {
    do_pmaxIPnum0(x)
  }
}

#' @rdname pmaxC
#' @export pminC
pminC <- function(x, a = 0) {
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
  do_pminC(x, a)
}


