#' Convenience function for coalescing to zero
#' @param x An atomic vector. Or a list for \code{COALESCE0}.
#' @param nThread Number of threads to use.
#'
#' @return Equivalent to \code{hutils::coalesce(x, 0)} for
#' an appropriate type of zero. \code{COALESCE0(x)}
#'
#' For complex numbers, each component is coalesced. For unsupported
#' types, the vector is returned, silently.
#'
#' @examples
#' coalesce0(c(NA, 2:3))
#' coalesce0(NaN + 1i)
#'
#' @export


coalesce0 <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
  .Call("Ccoalesce0", x, nThread, PACKAGE = "hutilscpp")
}

#' @rdname coalesce0
#' @export
COALESCE0 <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
  if (is.atomic(x)) {
    return(x)
  }
  if (is.data.frame(x)) {
    # nocov start
    if (!is.data.table(x)) {
      message("Coercing to data.table")
      setDT(x)
    }
    # nocov end
    for (j in seq_along(x)) {
      set(x, j = j, value = coalesce0(.subset2(x, j)))
    }
    return(x)
  }
  lapply(x, coalesce0)
}

uncoalesce0 <- function(x) {
  .Call("Cuncoalesce0", x, PACKAGE = "hutilscpp")
}
