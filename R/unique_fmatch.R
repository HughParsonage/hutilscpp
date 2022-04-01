#' Distinct elements
#' @description Using the \code{fastmatch} hash functions, determine
#' the unique elements of a vector, and the number of distinct elements.
#'
#' @param x An atomic vector.
#' @param nThread Number of threads to use.
#'
#' @return
#' Equivalent to \code{unique(x)} or \code{data.table::uniqueN(x)} respectively.
#'
#' @export

unique_fmatch <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
  fmatchx <- fmatchp(x, x, nThread = nThread, nomatch = 0L)
  .Call("Cunique_fmatch", x, fmatchx, nThread, PACKAGE = "hutilscpp") %||%
    unique(x) # nocov
}

#' @rdname unique_fmatch
#' @export
uniqueN_fmatch <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
  fmatchx <- fmatchp(x, x, nThread = nThread, nomatch = 0L)
  .Call("CuniqueN_fmatch", fmatchx, nThread, PACKAGE = "hutilscpp") %||%
    length(unique(x)) # nocov
}
