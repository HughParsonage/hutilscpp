#' Parallel fastmatching
#' @description \code{fastmatch::fmatch} and logical versions, with parallelization.
#' @param x,table,nomatch As in \code{match}.
#' @param nThread Number of threads to use.
#' @param fin \code{TRUE | FALSE} Behaviour of return value when value found in
#' \code{table}. If \code{FALSE}, return the index of \code{table};
#' if \code{TRUE}, return \code{TRUE}.
#'
#' @examples
#' x <- c(1L, 4:5)
#' y <- c(2L, 4:5)
#' fmatchp(x, y)
#' fmatchp(x, y, nomatch = 0L)
#' finp(x, y)
#'
#' @export
fmatchp <- function(x, table, nomatch = NA_integer_, nThread = getOption("hutilscpp.nThread", 1L), fin = FALSE) {
  check_omp(nThread)
  stopifnot(is.integer(nomatch))
  check_TF(fin)
  .Call("fmatch", x, table, nomatch, FALSE, fin, nThread, PACKAGE = packageName())
}

#' @rdname fmatchp
#' @export
finp <- function(x, table, nThread = getOption("hutilscpp.nThread", 1L)) {
  stopifnot(is.integer(x), is.integer(table))
  nThread <- ensure_integer(nThread)
  fmatchp(x, table, nomatch = 0L, nThread = nThread, fin = TRUE)
}
