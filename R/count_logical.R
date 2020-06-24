#' Count logicals
#' @description Count the number of \code{FALSE}, \code{TRUE}, and \code{NA}s.
#' @param x A logical vector.
#'
#' @return A vector of 3 elements: the number of \code{FALSE}, \code{TRUE}, and
#' \code{NA} values in \code{x}.
#'
#' @export

count_logical <- function(x) {
  if (!is.logical(x)) {
    stop(g("`x` was type {typeof(x)}, but must be type logical."))
  }
  if (length(x) <= .Machine$integer.max) {
    do_count_logical(x)
  } else {
    do_count_logical_long(x)
  }
}
