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

