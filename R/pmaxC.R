#' @title Parallel maximum/minimum
#' @description Faster \code{pmax()} and \code{pmin()}.
#'
#' @name pmaxC
#' @aliases pminC pmaxV pminV pmax0 pmin0
#' @param x \describe{
#' \item{\code{numeric(n)}}{A numeric vector.}
#' }
#' @param y,z \describe{\item{\code{numeric(n)}}{Other numeric vectors the same length as \code{x}}}
#' @param a \describe{\item{\code{numeric(1)}}{A single numeric value.}}
#'
#' @param in_place \describe{
#' \item{\code{TRUE | FALSE}, default: \code{FALSE}}{Should \code{x} be modified in-place? For advanced use only.}
#' }
#'
#'
#' @param keep_nas \describe{
#' \item{\code{TRUE | FALSE}, default: \code{FALSE}}{Should \code{NA}s values be
#' preserved? By default, \code{FALSE}, so the behaviour of the function is
#' dependent on the representation of \code{NA}s at the C++ level.}
#' }
#'
#' @param dbl_ok \describe{
#' \item{\code{TRUE | FALSE}, default: \code{TRUE}}{Is it acceptable to return
#' a non-integer vector if \code{x} is integer? If \code{TRUE}, the default,
#' if \code{x} is an integer vector, a double vector may be returned if
#' \code{a} is not an integer.}
#' }
#'
#' @param sorted \describe{
#' \item{\code{TRUE | FALSE}, default: \code{FALSE}}{
#' Is \code{x} known to be sorted?
#' If \code{TRUE}, \code{x} is assumed to be sorted. Thus the
#'  first zero determines whether the position at which zeroes start or end.}
#' }
#'
#' @param nThread \describe{
#' \item{\code{integer(1)}}{The number of threads to use. Combining \code{nThread > 1}
#' and \code{in_place = TRUE} is not supported.}
#' }
#'
#' @return Versions of \code{pmax} and \code{pmin}, designed for performance.
#'
#' When \code{in_place = TRUE}, the values of \code{x} are modified in-place.
#' For advanced users only.
#'
#'
#'
#'
#'
#' The differences are:
#' \describe{
#' \item{\code{pmaxC(x, a)} and \code{pminC(x, a)}}{Both \code{x} and \code{a} must be numeric and
#' \code{a} must be length-one.}
#' }
#'
#'
#'
#'
#' @note This function will always be faster than \code{pmax(x, a)} when \code{a} is
#'  a single value, but can be slower than \code{pmax.int(x, a)} when \code{x} is short.
#'  Use this function when comparing a numeric vector with a single value.
#'
#'  Use \code{in_place = TRUE} only within functions when you are sure it is safe, i.e. not a
#'  reference to something outside the environment.
#'
#'  By design, the functions first check whether \code{x} will be modified before
#'  allocating memory to a new vector. For example, if all values in \code{x} are
#'  nonnegative, the vector is returned.
#'
#'
#' @examples
#' pmaxC(-5:5, 2)
#'
#' @export pmaxC pmax0 pmaxV pmax3
#'
#'

pmaxC <- function(x, a,
                  in_place = FALSE,
                  keep_nas = FALSE,
                  dbl_ok = NA,
                  nThread = getOption("hutilscpp.nThread", 1L)) {
  if (msg_dbl_ok <- anyNA(dbl_ok)) {
    dbl_ok <- TRUE
  }
  check_TF(dbl_ok)

  check_TF(in_place)
  check_TF(keep_nas)
  nThread <- check_omp(nThread)
  if (!is.atomic(x) || !is.numeric(x)) {
    stop("\n`x` was of type ", typeof(x), ", class ", toString(class(x)), " and\n",
         "`a` was of type ", typeof(a), ", class ", toString(class(a)), " and\n",
         "Both `x` and `a` must be atomic numeric vectors.")
  }
  if (amsg <- isnt_number(a, na.bad = TRUE, infinite.bad = TRUE, int.only = !dbl_ok)) {
    stop(attr(amsg, "ErrorMessage"))
  }
  x_was_integer <- is.integer(x)
  if (is.double(x)) {
    a <- as.double(a)
  } else if (!dbl_ok) {
    a <- ensure_integer(a)
  }
  if (!length(x)) {
    return(x)
  }

  if (in_place && is.symbol(substitute(x))) {
    o <- .Call("CpmaxC_in_place", x, a, keep_nas, nThread, PACKAGE = packageName())
  } else {
    o <- .Call("Cpmax", x, a, keep_nas, nThread, PACKAGE = packageName())
  }
  if (is.null(o)) {
    o <- pmax.int(x, a)
  }
  if (x_was_integer && is.double(o) && msg_dbl_ok) {
    message("Output is double")
  }

  return(o)
}

#' @rdname pmaxC
#' @export
pminC <- function(x, a,
                  in_place = FALSE,
                  keep_nas = FALSE,
                  dbl_ok = TRUE,
                  nThread = getOption("hutilscpp.nThread", 1L)) {
  if (msg_dbl_ok <- anyNA(dbl_ok)) {
    dbl_ok <- TRUE
  }
  check_TF(in_place)
  check_TF(keep_nas)
  nThread <- check_omp(nThread)
  if (!is.atomic(x) || !is.numeric(x)) {
    stop("\n`x` was of type ", typeof(x), ", class ", toString(class(x)), " and\n",
         "`a` was of type ", typeof(a), ", class ", toString(class(a)), " and\n",
         "Both `x` and `a` must be atomic numeric vectors.")
  }
  if (amsg <- isnt_number(a, na.bad = TRUE, infinite.bad = TRUE, int.only = !dbl_ok)) {
    stop(attr(amsg, "ErrorMessage"))
  }
  x_was_integer <- is.integer(x)
  if (is.double(x)) {
    a <- as.double(a)
  } else if (!dbl_ok) {
    a <- ensure_integer(a)
  }
  if (!length(x)) {
    return(x)
  }

  if (in_place && is.symbol(substitute(x))) {
    o <- .Call("CpminC_in_place", x, a, keep_nas, nThread, PACKAGE = packageName())
  } else {
    o <- .Call("Cpmin", x, a, keep_nas, nThread, PACKAGE = packageName())
  }
  if (is.null(o)) {
    o <- pmin.int(x, a)
  }
  if (x_was_integer && is.double(o) && msg_dbl_ok) {
    message("Output is double")
  }
  .Call("Cpmin", x, a, keep_nas, nThread, PACKAGE = packageName())
}

#' @rdname pmaxC
#' @export
pmax0 <- function(x, in_place = FALSE, sorted = FALSE, keep_nas = FALSE, nThread = getOption("hutilscpp.nThread", 1L)) {
  if (!is.atomic(x) || !is.numeric(x)) {
    stop("`x` was a ", class(x), ", but must be numeric.")
  }
  if (!length(x)) {
    return(x)
  }
  check_TF(in_place)
  check_TF(sorted)
  check_TF(keep_nas)
  nThread <- check_omp(nThread)

  if (is_altrep(x)) {
    if (in_place) {
      warning("`in_place = TRUE`, but `x` is an ALTREP vector. ",
              "This is unsupported so `in_place` is being set to FALSE.")
    }
    return(do_pminmax0_altrep(x, do_pmin = FALSE, keep_nas = keep_nas, nThread = nThread))
  }

  if (sorted) {
    if (is.integer(x)) {
      return(do_pmax0_radix_sorted_int(x, in_place = in_place))
    } else {
      return(do_pmax0_radix_sorted_dbl(x, in_place = in_place))
    }
  }

  if (is.integer(x) && !keep_nas) {
    return(do_pmax0_bitwise(x, in_place = in_place, nThread = nThread))
  }
  z <- if (is.double(x)) 0 else 0L
  if (in_place) {
    .Call("CpmaxC_in_place", x, z, keep_nas, nThread, PACKAGE = packageName())
  } else {
    .Call("Cpmax", x, z, keep_nas, nThread, PACKAGE = packageName())
  }
}

#' @rdname pmaxC
#' @export
pmin0 <- function(x, in_place = FALSE, sorted = FALSE, keep_nas = FALSE, nThread = getOption("hutilscpp.nThread", 1L)) {
  if (!is.atomic(x) || !is.numeric(x)) {
    stop("`x` was a ", class(x), ", but must be numeric.")
  }
  if (!length(x)) {
    return(x)
  }
  check_TF(in_place)
  check_TF(sorted)
  check_TF(keep_nas)
  nThread <- check_omp(nThread)

  if (is_altrep(x)) {
    if (in_place) {
      warning("`in_place = TRUE`, but `x` is an ALTREP vector. ",
              "This is unsupported so `in_place` is being set to FALSE.")
    }
    return(do_pminmax0_altrep(x, do_pmin = TRUE, keep_nas = keep_nas, nThread = nThread))
  }

  if (sorted) {
    if (is.integer(x)) {
      return(do_pmin0_radix_sorted_int(x, in_place = in_place))
    } else {
      return(do_pmin0_radix_sorted_dbl(x, in_place = in_place))
    }
  }

  if (is.integer(x) && !keep_nas) {
    return(do_pmin0_bitwise(x, in_place = in_place, nThread = nThread))
  }

  z <- if (is.double(x)) 0 else 0L
  if (in_place && is.symbol(substitute(x))) {
    .Call("CpminC_in_place", x, z, keep_nas, nThread, PACKAGE = packageName())
  } else {
    .Call("Cpmin", x, z, keep_nas, nThread, PACKAGE = packageName())
  }
}

#' @rdname pmaxC
#' @export
pmaxV <- function(x, y, in_place = FALSE, dbl_ok = TRUE, nThread = getOption("hutilscpp.nThread", 1L)) {
  check_TF(in_place)
  check_TF(dbl_ok)
  nThread <- check_omp(nThread)
  if (length(x) != length(y)) {
    stop("`length(x) = ", length(x), "`, yet ",
         "`length(y) = ", length(y), "`. ",
         "`x` and `y` must have the same length.")
  }
  if (!is.atomic(x) || !is.atomic(y) || !is.numeric(x) || !is.numeric(y)) {
    stop("\n`x` was of type ", typeof(x), ", class ", toString(class(x)), " and\n",
         "`y` was of type ", typeof(y), ", class ", toString(class(y)), " and\n",
         "Both `x` and `y` must be atomic numeric vectors.")
  }

  if (in_place) {
    swap_xy <- FALSE
  } else {
    swap_xy <- is.integer(y) && is.double(x)
  }
  out <- .Call("Cpmax", x, y, TRUE, nThread, PACKAGE = packageName())
  if (in_place && is.symbol(substitute(x))) {
    eval.parent(substitute(x <- out))
  }
  out
}

#' @rdname pmaxC
#' @export
pminV <- function(x, y, in_place = FALSE, dbl_ok = TRUE, nThread = getOption("hutilscpp.nThread", 1L)) {
  check_TF(in_place)
  check_TF(dbl_ok)
  nThread <- check_omp(nThread)
  if (length(x) != length(y)) {
    stop("`length(x) = ", length(x), "`, yet ",
         "`length(y) = ", length(y), "`. ",
         "`x` and `y` must have the same length.")
  }
  if (!is.atomic(x) || !is.atomic(y) || !is.numeric(x) || !is.numeric(y)) {
    stop("\n`x` was of type ", typeof(x), ", class ", toString(class(x)), " and\n",
         "`y` was of type ", typeof(y), ", class ", toString(class(y)), " and\n",
         "Both `x` and `y` must be atomic numeric vectors.")
  }

  out <- .Call("Cpmin", x, y, TRUE, nThread, PACKAGE = packageName())
  if (in_place && is.symbol(substitute(x))) {
    eval.parent(substitute(x <- out))
  }
  out
}

#' @rdname pmaxC
#' @export
pmax3 <- function(x, y, z, in_place = FALSE) {
  .pminpmax3(x, y, z, in_place = in_place, do_max = TRUE)
}

#' @rdname pmaxC
#' @export
pmin3 <- function(x, y, z, in_place = FALSE) {
  .pminpmax3(x, y, z, in_place = in_place, do_max = FALSE)
}

.pminpmax3 <- function(x, y, z, in_place = FALSE, do_max) {
  check_TF(in_place)
  check_TF(do_max)
  lx <- length(x)
  nThread <- check_omp(getOption("hutilscpp.nThread", 1L))
  if (length(y) == lx && length(z) == lx) {
    out <- .Call("Csummary3", x, y, z, ifelse(do_max, "max", "min"), nThread, PACKAGE = packageName())
    if (!is.null(out)) {
      return(out)
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
      if (AND(is.double(y),
              wb <- which_isnt_integerish(y))) {
        stop("`x` was type integer and `y` was type double, but entry ", wb,
             " was not equal to the integer equivalent. ")
      }
    }
    if (is.double(z)) {
      if (AND(is.double(z),
              wb <- which_isnt_integerish(z))) {
        stop("`x` was type integer and `z` was type double, but entry ", wb,
             " was not equal to the integer equivalent. ")
      }
    }

    out <- .Call("Csummary3", x, as.integer(y), as.integer(z), ifelse(do_max, "max", "min"), nThread, PACKAGE = packageName())
    if (is.null(out)) {
      if (do_max) {
        return(pmax.int(x, pmax.int(y, z)))
      } else {
        return(pmin.int(x, pmin.int(y, z)))
      }
    }
    return(out)
  }
  if (is.double(x) && is.numeric(y) && is.numeric(z)) {
    nThread <- check_omp(getOption("hutilscpp.nThread", 1L))
    out <- .Call("Csummary3", x, as.double(y), as.double(z), ifelse(do_max, "max", "min"), nThread, PACKAGE = packageName())
    if (is.null(out)) {
      if (do_max) {
        return(pmax.int(x, pmax.int(y, z)))
      } else {
        return(pmin.int(x, pmin.int(y, z)))
      }
    }
    return(out)
  }
  # nocov begin
  if (do_max) {
    pmax(x, pmax(y, z))
  } else {
    pmin(x, pmin(y, z))
  }
  # nocov end
}


do_pminmax0_altrep <- function(x,
                               a = 0L,
                               keep_nas = FALSE,
                               do_pmin = FALSE,
                               nThread = getOption("hutilscpp.nThread", 1L)) {
  x1 <- x[1]
  n <- length(x)
  xn <- x[n]

  all_nonnegative <- x1 >= 0 && xn >= 0
  all_nonpositive <- x1 <= 0 && xn <= 0

  if (do_pmin && all_nonpositive) {
    return(x)
  }
  if (!do_pmin && all_nonnegative) {
    return(x)
  }
  if (!do_pmin && all_nonpositive) {
    return(allocate0_int(n, nThread = nThread))
  }
  if (do_pmin && all_nonnegative) {
    return(allocate0_int(n, nThread = nThread))
  }

  d <- x[2] - x[1]
  # Should zeroes be to the
  # left of the root or
  #  0  0  0  1  2  3  4
  # right?
  # -2 -1  0  0  0  0  0
  #
  zero_left <- XOR(do_pmin, x1 < 0)
  root <- (-x1 / d)
  allocate_with_root(n, a = a, root, zero_left, do_pmin = do_pmin, nThread = nThread)
}

do_pmax0_radix_sorted_int <- function(x, in_place = FALSE, nThread = getOption("hutilscpp.nThread", 1L)) {
  nThread <- check_omp(nThread)
  .Call("Cpmax0_radix_sorted_int", x, in_place, nThread, PACKAGE = packageName())
}

do_pmin0_radix_sorted_int <- function(x, in_place = FALSE, nThread = getOption("hutilscpp.nThread", 1L)) {
  nThread <- check_omp(nThread)
  .Call("Cpmin0_radix_sorted_int", x, in_place, nThread, PACKAGE = packageName())
}

do_pmax0_radix_sorted_dbl <- function(x, in_place = FALSE, nThread = getOption("hutilscpp.nThread", 1L)) {
  nThread <- check_omp(nThread)
  .Call("Cpmax0_radix_sorted_dbl", x, in_place, nThread, PACKAGE = packageName())
}

do_pmin0_radix_sorted_dbl <- function(x, in_place = FALSE, nThread = getOption("hutilscpp.nThread", 1L)) {
  nThread <- check_omp(nThread)
  .Call("Cpmin0_radix_sorted_dbl", x, in_place, nThread, PACKAGE = packageName())
}

do_pmin0_bitwise <- function(x, in_place = FALSE, nThread = getOption("hutilscpp.nThread", 1L)) {
  nThread <- check_omp(nThread)
  .Call("Cpmin0_bitwise", x, in_place, nThread, PACKAGE = packageName())
}

do_pmax0_bitwise <- function(x, in_place = FALSE, nThread = getOption("hutilscpp.nThread", 1L)) {
  nThread <- check_omp(nThread)
  .Call("Cpmax0_bitwise", x, in_place, nThread, PACKAGE = packageName())
}
