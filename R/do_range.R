#' Range C++
#' @param x A vector for which the range is desired.
#' @export range_rcpp

range_rcpp <- function(x) {
  if (is.integer(x)) {
    return(do_range_int(x))
  }
  if (is.double(x)) {
    return(do_range_dbl(x))
  }
  range(x)
}
