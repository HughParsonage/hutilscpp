#' Parallel minimum
#' @name pminC
#' @param x A numeric vector.
#' @param y,z Other numeric vectors.
#' @param a A single number.
#' @param in_place (logical, default: \code{FALSE}) Should \code{x}
#' be modified in-place.
#' @return Same as \code{pmin(x, 0)}.
#'
#' \code{pmin0(x) = pmin(x, 0)}
#'
#' \code{pminV(x, y) = pmin(x, y)}
#'
#' \code{pminC(x, a) = pmin(x, a)}  for length-one \code{a}.
#'
#' \code{pmin3(x, y, z) = pmin(x, pmin(y, z))}.
#'
#' @details
#' The type of \code{x} is preserved as far as possible.
#'
#' @examples
#' pminV(10:1, 1:10)
#' pmin0(-5:5)
#' seq_out <- function(x, y) seq(x, y, length.out = 10)
#' pmin3(seq_out(0, 10), seq_out(-5, 50), seq_out(20, -10))
#'
#' @export pmin0 pminV pminC pmin3


#' @rdname pminC
pmin0 <- function(x, in_place = FALSE) {
  check_TF(in_place)
  if (is.integer(x)) {
    do_pmin0_int(x, in_place = in_place)
  } else if (is.double(x)) {
    do_pmin0_dbl(x, in_place = in_place)
  } else {
    pmin(x, 0)
  }
}

#' @rdname pminC
pminV <- function(x, y, in_place = FALSE) {
  if (is.integer(x) && is.integer(y) && length(x) == length(y)) {
      return(do_pminV_int(x, y, in_place))
  }
  if (is.double(x) && is.double(y) && length(x) == length(y)) {
    return(do_pminV_dbl(x, y, in_place))
  }

  if (length(x) != length(y)) {
    stop("`length(x) = ", length(x), "`, yet ",
         "`length(y) = ", length(y), "`. ",
         "`x` and `y` must be the same length.")
  }
  if (in_place) {
    warning("`in_place = TRUE` but `x` and `y` were not numeric.")
  }
  pmin.int(x, y)
}

#' @rdname pminC
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

#' @rdname pminC
pmin3 <- function(x, y, z, in_place = FALSE) {
  check_TF(in_place)
  lx <- length(x)
  if (length(y) == lx && length(z) == lx) {
    if (is.integer(x) && is.integer(y) && is.integer(z)) {
      return(do_summary3_int(x, y, z, in_place, do_max = FALSE))
    }
    if (is.double(x) && is.double(y) && is.double(z)) {
      return(do_summary3_dbl(x, y, z, in_place, do_max = FALSE))
    }
  } else {
    if (length(y) != lx) {
      if (length(y) != 1L) {
        stop("`y` had length ", length(y), ", yet ",
             "`x` had length ", length(x), ". ",
             "`y` and `z` must be the same length as `x`, (or length-one).")
      }

      x <- pminC(x, y, in_place = in_place)
    } else {
      x <- pminV(x, y, in_place = in_place)
    }
    if (length(z) != lx) {
      if (length(z) != 1L) {
        stop("`z` had length ", length(z), ", yet ",
             "`x` had length ", length(x), ". ",
             "`y` and `z` must be the same length as `x`, (or length-one).")
      }
      return(pminC(x, z, in_place = in_place))
    } else {
      return(pminV(x, z, in_place = in_place))
    }
  }

  if (is.integer(x) && (is.double(y) || is.double(z))) {
    yi <- as.integer(y)
    if (is.double(y) && max(abs(y - yi)) > sqrt(.Machine$double.eps)) {
      wb <- which.max(abs(y - yi)) > sqrt(.Machine$double.eps)
      stop("`x` was type 'integer' and `y` was type double, but entry ", wb,
           " was not equal to the integer equivalent. ", )
    }
    zi <- as.integer(z)
    if (is.double(z) && max(abs(z - zi)) > sqrt(.Machine$double.eps)) {
      wb <- which.max(abs(z - zi)) > sqrt(.Machine$double.eps)
      stop("`x` was type 'integer' and `z` was type double, but entry ", wb,
           " was not equal to the integer equivalent. ", )
    }
    return(do_summary3_int(x, y, z, in_place = in_place, do_max = FALSE))
  }
  if (is.double(x) && is.numeric(y) && is.numeric(z)) {
    y <- as.double(y)
    z <- as.double(z)
    return(do_summary3_dbl(x, y, z, in_place = in_place, do_max = FALSE))
  }

  if (!is.numeric(x) || !is.numeric(y) || !is.numeric(z)) {
    stop("`x` was of type ", typeof(x),
         "`y` was of type ", typeof(y),
         "`z` was of type ", typeof(z), ". ",
         "All of `x`, `y`, and `z` must be numeric.")
  }
}


