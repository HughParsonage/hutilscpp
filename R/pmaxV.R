#' Parallel maximum of equi-length vectors
#' @param x,y Numeric vectors of equal length and same type.
#' @export pmaxV

pmaxV <- function(x, y) {
  if (is.integer(x) && is.integer(y)) {
    return(do_pmaxIntInt(x, y))
  }
  if (is.double(x) && is.double(y)) {
    return(do_pmaxNumNum(x, y))
  }
  if (is.integer(y)) {
    stop("`y` is type integer, yet `x` is type double.")
  }
  if (is.double(y)) {
    stop("`y` is type double, yet `x` is type integer.")
  }
}


