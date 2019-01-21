#' Range C++
#' @param x A vector for which the range is desired. Missing values
#' have no defined behaviour.
#' @return A vector, the first two positions give the range and
#' the next two give the positions where the max and min occur.
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
