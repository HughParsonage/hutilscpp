#' Negative component
#' @param x A vector.
#' @return Same as \code{pmin(x, 0)}.
#'
#' @examples
#' pmin0(-5:5)
#'
#' @export

pmin0 <- function(x) {
  if (is.integer(x)) {
    pmin.int(x, 0L)
  } else if (is.double(x)) {
    do_pmin0(x)
  } else {
    pmin(x, 0)
  }
}

pminV <- function(x, y) {
  if (is.double(x) && is.double(y) && length(x) == length(y)) {
    do_pminV(x, y)
  } else {
    if (length(x) != length(y)) {
      stop("`length(x) = ", length(x), "`, yet ",
           "`length(y) = ", length(y), "`. ",
           "`x` and `y` must be the same length.")
    }
    pmin.int(x, y)
  }
}

