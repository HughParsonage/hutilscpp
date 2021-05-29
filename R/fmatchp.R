#' Parallel fastmatching
#' @description \code{fastmatch::fmatch} and logical versions, with parallelization.
#' @param x,table,nomatch As in \code{match}.
#' @param nThread Number of threads to use.
#' @param fin \code{TRUE | FALSE} Behaviour of return value when value found in
#' \code{table}. If \code{FALSE}, return the index of \code{table};
#' if \code{TRUE}, return \code{TRUE}.
#'
#' @param whichFirst \code{integer(1)} If \code{0L}, not used. If positive,
#' returns the index of the first element in \code{x} found in \code{table};
#' if negative, returns the last element in \code{x} found in \code{table}.
#'
#' @examples
#' x <- c(1L, 4:5)
#' y <- c(2L, 4:5)
#' fmatchp(x, y)
#' fmatchp(x, y, nomatch = 0L)
#' finp(x, y)
#'
#' @export
fmatchp <- function(x, table, nomatch = NA_integer_, nThread = getOption("hutilscpp.nThread", 1L), fin = FALSE,
                    whichFirst = 0L) {
  nThread <- check_omp(nThread)
  stopifnot(is.integer(nomatch))
  check_TF(fin)
  if (!is.symbol(substitute(table))) {
    # avoid constants
    table <- copy(table)
  }
  .Call("fmatch", x, table, nomatch, fin, whichFirst, nThread, PACKAGE = packageName)
}

#' @rdname fmatchp
#' @export
finp <- function(x, table, nThread = getOption("hutilscpp.nThread", 1L)) {
  nThread <- check_omp(nThread)
  fmatchp(x, table, nomatch = 0L, nThread = nThread, fin = TRUE)
}

#' @rdname fmatchp
#' @export
fnotinp <- function(x, table, nThread = getOption("hutilscpp.nThread", 1L)) {
  nThread <- check_omp(nThread)
  ans <- fmatchp(x, table, nomatch = 0L, nThread = nThread, fin = TRUE)
  FLIP(ans);
}
FLIP <- `!`
do_par_in_hash_int <- finp
do_par_in_hash_dbl <- finp
