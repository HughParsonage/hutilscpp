
which_NA <- function(x) {
  ans <- .Call("Cwhich_isna", x, FALSE, 1L, PACKAGE = packageName())
  if (is.null(ans)) {
    return(seq_along(x))
  }
  ans
}

which_notNA <- function(x) {
  ans <- .Call("Cwhich_isna", x, TRUE, 1L, PACKAGE = packageName())
  if (is.null(ans)) {
    return(seq_along(x))
  }
  ans
}

