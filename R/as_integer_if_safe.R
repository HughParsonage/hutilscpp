#' Coerce from double to integer if safe
#' @description The same as \code{as.integer(x)} but only if \code{x} consists only of
#' whole numbers and is within the range of integers.
#'
#' @param x A double vector. If not a double vector, it is simply returned without any coercion.
#' @export
#'

as_integer_if_safe <- function(x) {
  if (is.double(x) && is_safe2int(x, as.double(.Machine$integer.max))) {
    return(force_as_integer(x))
  }
  x
}


