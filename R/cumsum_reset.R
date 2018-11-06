#' Cumulative sum unless reset
#' @param x A logical vector indicating when the sum should \emph{continue}
#' @param y Optional: a numeric vector the same length as \code{x} to cumulatively sum.
#' @return If \code{y} is a double vector, a double vector of cumulative sums,
#' resetting whenever \code{x} is \code{FALSE}; otherwise an integer vector.
#'
#' @export

cumsum_reset <- function(x, y = x) {
  if (!is.logical(x)) {
    stop("`x` must be a logical vector.")
  }
  if (anyNA(x)) {
    stop("`x` has missing values. This is not permitted.")
  }
  if (missing(y)) {
    return(do_cumsum_reset_logical(x))
  }
  if (length(y) != length(x)) {
    stop("`y` had length ", length(y), ", yet `length(x) = ", length(x), ". ",
         "`x` and `y` must have the same lengths. ",
         "Provide vectors of equal length.")
  }
  if (is.logical(y)) {
    y <- as.integer(y)
  }
  if (is.integer(y)) {
    return(do_cumsum_reset_integer(x, y))
  }
  if (is.double(y)) {
    return(do_cumsum_reset_double(x, y))
  }
  stop("`y` was type ", class(y), ", but must be an integer or double.")
}


