#' @title Squish into a range
#' @name squish
#' @param x A numeric vector
#' @param a,b Upper and lower bounds
#'
#' @export

squish <- function(x, a, b) {
  if (length(a) != 1L) {
    stop("`length(a) = ", length(a), "`, but must be length-one.")
  }
  if (length(b) != 1L) {
    stop("`length(b) = ", length(b), "`, but must be length-one.")
  }
  if (length(x) < 1e3L) {
    return(pmax.int(a, pmin.int(x, b)))
  }
  if (is.integer(x)) {
    squishi(x, as.integer(a), as.integer(b))
  } else {
    squishn(x, a, b)
  }
}
