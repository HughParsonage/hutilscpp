#' Is a vector sorted?
#'
#' @param x An atomic vector.
#' @param asc Single logical. If \code{NA}, the default, a vector is considered
#' sorted if it is either sorted ascending or sorted descending;
#' if \code{FALSE}, a vector is sorted only if sorted descending;
#' if \code{TRUE}, a vector is sorted only if sorted ascending.
#'
#' @return \code{is_sorted} returns \code{TRUE} or \code{FALSE}
#'
#' \code{isntSorted} returns \code{0} if sorted or the first position
#' that proves the vector is not sorted
#'
#'
#' @export

is_sorted <- function(x, asc = NA) {
  is.null(x) || {
    stopifnot(is.atomic(x),
              is.logical(asc),
              length(asc) == 1L)
    ans <- .Call("Cis_sorted", x, asc, PACKAGE = packageName)
    if (is.null(ans)) {
      return(!is.unsorted(x)) # nocov
    }
    ans
  }
}

#' @rdname is_sorted
#' @export
isntSorted <- function(x, asc = NA) {
  if (is.null(x)) {
    return(0L) # nocov
  }
  stopifnot(is.atomic(x),
            is.logical(asc),
            length(asc) == 1L)
  ans <- .Call("Cisnt_sorted", x, asc, PACKAGE = packageName)
  if (is.null(ans)) {
    stop("`x` was type ", typeof(x), ", which is not yet supported.") # nocov
  }
  ans
}


string_equals <- function(x, y) {
  .Call("CStringEqual", x, y, PACKAGE = packageName)
}


