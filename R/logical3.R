#' Vectorized logical with support for short-circuits
#' @name logical3
#' @param x,y,z Logical vectors. If \code{z} is \code{NULL} it is interpreted as \code{TRUE} or
#' \code{FALSE} recycled to the length of \code{x}.
#' @export and3 or3
#'

and3 <- function(x, y, z = NULL) {
  if (length(x) != length(y)) {
    stop("`length(x) = ",
         length(x), "``, yet",
         "`length(y) = ",
         length(y), "`.")
  }
  if (is.null(z)) {
    z <- logical(0)
  }
  do_and3(x, y, z)
}

#' @rdname logical3
or3 <- function(x, y, z = NULL) {
  if (length(x) != length(y)) {
    stop("`length(x) = ",
         length(x), "``, yet",
         "`length(y) = ",
         length(y), "`.")
  }
  if (is.null(z)) {
    z <- logical(0)
  }
  do_or3(x, y, z)
}



