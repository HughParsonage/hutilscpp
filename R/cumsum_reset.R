#' Cumulative sum unless reset
#' @param x A logical vector indicating when the sum should \emph{continue}.
#' Missing values in \code{x} is an error.
#' @param y Optional: a numeric vector the same length as \code{x} to cumulatively sum.
#' @return A vector of cumulative sums,
#' resetting whenever \code{x} is \code{FALSE}.
#' The return type is double if \code{y} is double; otherwise an integer vector. Integer
#' overflow wraps around, rather than being promoted to double type, as this
#' function is intended for 'shortish' runs of cumulative sums.
#'
#' If \code{length(x) == 0}, \code{y} is returned (i.e. \code{integer(0)} or \code{double(0)}.
#'
#' @examples
#' cumsum_reset(c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE))
#' cumsum_reset(c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE),
#'              c(1000, 1000, 10000,   10,   20,   33,     0))
#'
#' @export

cumsum_reset <- function(x, y = as.integer(x)) {
  if (!is.logical(x)) {
    stop("`x` must be a logical vector.")
  }
  if (anyNA(x)) {
    stop("`x` has missing values. This is not permitted.")
  }
  if (missing(y)) {
    if (length(x)) {
      return(.Call("Ccumsum_reset", x, NULL, PACKAGE = packageName))
    } else {
      return(integer(0L))
    }
  }
  if (length(y) != length(x)) {
    stop("`y` had length ", length(y), ", yet `length(x) = ", length(x), ". ",
         "`x` and `y` must have the same lengths. ",
         "Provide vectors of equal length.")
  }
  if (!length(y)) {
    return(y)
  }
  if (!is.numeric(y)) {
    stop("`y` was type ", class(y), ", but must be an integer or double.")
  }
  .Call("Ccumsum_reset", x, y, PACKAGE = packageName)
}


cumsum_reset_where <- function(where, y, .parent_nframes = 1L) {
  sexpr <- substitute(where)
  isBinaryW <- is_binary_sexp(sexpr, .parent_nframes = .parent_nframes + 1L)
  if (isBinaryW) {
    x <- eval.parent(sexpr[[2]], n = .parent_nframes)
    o <- attr(isBinaryW, "M")
    rhs_eval <- attr(isBinaryW, "rhs_eval")
    ans <- .Call("Ccumsum_reset_where", x, y, o, rhs_eval, PACKAGE = packageName)
    if (!is.null(ans)) {
      return(ans)
    }
  }
  cumsum_reset(where, y)
}

