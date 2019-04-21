#' Vectorized logical with support for short-circuits
#' @name logical3
#' @param x,y,z Logical vectors. If \code{z} is \code{NULL} the function is equivalent to the
#' binary versions; only \code{x} and \code{y} are used.
#' @param nas_absent (logical, default: \code{FALSE}) Can it be assumed that \code{x,y,z} have
#' no missing values? Set to \code{TRUE} when you are sure that that is the case; setting to
#' \code{TRUE} falsely has no defined behaviour.
#' @return For \code{and3}, the same as \code{x & y & z};
#' for \code{or3}, the same as \code{x | y | z}, designed to be efficient when component-wise
#' short-circuiting is available.
#' @export and3 or3
#'

and3 <- function(x, y, z = NULL, nas_absent = FALSE) {
  .z <- if (is.null(z)) TRUE else z
  stopifnot(is.logical(x),
            is.logical(y),
            is.logical(.z))
  lx <- length(x)
  ly <- length(y)
  lz <- length(.z)

  if (lx == ly && ly == lz && isTRUE(nas_absent)) {
    return(do_and3(x, y, z))
  }


  max.length <- max(lx, ly, lz)
  if (max.length == 1L) {
    return(x && y && z)
  }
  if (lx != 1L && lx != max.length) {
    stop("`length(x) = ", lx, ", yet ",
         if (ly == 1L) {
           "`length(z) = "
         } else {
           "`length(y) = "
         },
         max.length, ". ",
         "The only permissible vector lengths are 1 or the maximum length of the inputs.")
  }
  if (ly != 1L && ly != max.length) {
    stop("`length(y) = ", ly, ", yet ",
         if (lx == 1L) {
           "`length(z) = "
         } else {
           "`length(x) = "
         },
         max.length, ". ",
         "The only permissible vector lengths are 1 or the maximum length of the inputs.")
  }
  if (!is.null(z) && lz != 1L && lz != max.length) {
    stop("`length(z) = ", lz, ", yet ",
         if (lx == 1L) {
           "`length(y) = "
         } else {
           "`length(x) = "
         },
         max.length, ". ",
         "The only permissible vector lengths are 1 or the maximum length of the inputs.")
  }

  # FALSE & <anything> always FALSE

  # Tricky style decisions: this is cleaner in this case but the length-1 cases
  # have lots of dangling elses
  # if (isFALSE(x) || isFALSE(y) || isFALSE(.z)) {
  #   return(logical(max.length))
  # }





  if (lx == 1L) {
    if (anyNA(x)) {
      if (is.null(z) || isTRUE(.z)) {
        return(na_and(y))
      } else {
        return(NA & y & z)
      }
    } else if (x) {
      if (is.null(z)) {
        return(y)
      } else if (lz == 1L) {
        if (anyNA(z)) {
          return(NA & y)
        } else if (z) {
          return(y)
        } else {
          # <anything> & FALSE => always false
          return(logical(max.length))
        }
      } else {
        # fall through
      }
    } else {
      # x is FALSE
      return(logical(max.length))
    }
  } else if (ly == 1L) {
    # but lx != 1L
    if (anyNA(y)) {
      if (is.null(z)) {
        return(na_and(x))
      } else {
        return(x & y & .z)
      }
    } else if (y) {
      if (lz == 1L) {
        if (anyNA(z)) {
          return(na_and(z))
        } else if (z) {
          return(x)
        } else {
          # <anything> & FALSE => always false
          return(logical(max.length))
        }
      } else {
       # fall through
      }
    } else {
      # y FALSE
      return(logical(max.length))
    }
  } else if (lz == 1L) {
    if (anyNA(.z)) {
      return(na_and(do_and3(x, y, TRUE)))
    } else if (.z) {
      # do nothing, allow following to handle
    } else {
      return(logical(max.length))
    }
  }


  # do_and3 has no capacity for nas
  if (anyNA(x) || anyNA(y) || anyNA(.z)) {
    # give up
    return(x & y & .z)
  }

  do_and3(x, y, .z)
}

#' @rdname logical3
or3 <- function(x, y, z = NULL) {
  .z <- if (is.null(z)) FALSE else z
  stopifnot(is.logical(x),
            is.logical(y),
            is.logical(.z))
  lx <- length(x)
  ly <- length(y)
  lz <- length(.z)
  max.length <- max(lx, ly, lz)
  if (max.length == 1L) {
    return(x || y || .z)
  }
  if (lx != 1L && lx != max.length) {
    stop("`length(x) = ", lx, ", yet ",
         if (ly == 1L) {
           "`length(z) = "
         } else {
           "`length(y) = "
         },
         max.length, ". ",
         "The only permissible vector lengths are 1 or the maximum length of the inputs.")
  }
  if (ly != 1L && ly != max.length) {
    stop("`length(y) = ", ly, ", yet ",
         if (lx == 1L) {
           "`length(z) = "
         } else {
           "`length(x) = "
         },
         max.length, ". ",
         "The only permissible vector lengths are 1 or the maximum length of the inputs.")
  }
  if (!is.null(z) && lz != 1L && lz != max.length) {
    stop("`length(z) = ", lz, ", yet ",
         if (lx == 1L) {
           "`length(y) = "
         } else {
           "`length(x) = "
         },
         max.length, ". ",
         "The only permissible vector lengths are 1 or the maximum length of the inputs.")
  }

  if (anyNA(x) || anyNA(y) || anyNA(.z)) {
    return(x | y | .z)
  }

  do_or3(x, y, .z)
}



