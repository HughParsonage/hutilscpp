#' Negative component
#' @param x A vector.
#' @param in_place (logical, default: \code{FALSE}) Should \code{x}
#' be modified in-place.
#' @return Same as \code{pmin(x, 0)}.
#'
#' @examples
#' pmin0(-5:5)
#'
#' @export

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

