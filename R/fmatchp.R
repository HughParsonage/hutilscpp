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
#' @param .raw \code{integer(1)}
#' \describe{
#' \item{0}{Return integer or logical as required.}
#' \item{1}{Return raw if possible.}
#' }
#'
#' @examples
#' x <- c(1L, 4:5)
#' y <- c(2L, 4:5)
#' fmatchp(x, y)
#' fmatchp(x, y, nomatch = 0L)
#' finp(x, y)
#'
#' @export
fmatchp <- function(x, table, nomatch = NA_integer_,
                    nThread = getOption("hutilscpp.nThread", 1L),
                    fin = FALSE,
                    whichFirst = 0L,
                    .raw = 0L) {
  nThread <- check_omp(nThread)
  if (is.logical(x)) {
    ans <- .Call("fmatchp_lgl", x, as.logical(table), nThread, fin, PACKAGE = "hutilscpp")
    if (is.null(ans)) {
      return(match_last_resort(x, table, nomatch, nThread, fin, whichFirst)) # nocov
    }
    if (is.na(nomatch) && is.integer(ans)) {
      .Call("Cuncoalesce0", ans, PACKAGE = "hutilscpp")
    }
    return(ans)
  }
  stopifnot(is.integer(nomatch))
  check_TF(fin)
  if (!is.symbol(substitute(table))) {
    # avoid constants
    table <- copy(table)
  }
  ans <- .Call("fmatch", x, if (is.raw(x)) as.raw(table) else table, nomatch, fin, whichFirst, nThread, PACKAGE = packageName)
  if (is.null(ans)) {
    return(match_last_resort(x, table, nomatch, nThread, fin, whichFirst)) # nocov
  }
  ans

}

#' @rdname fmatchp
#' @export
finp <- function(x, table, nThread = getOption("hutilscpp.nThread", 1L),
                 .raw = 0L) {
  nThread <- check_omp(nThread)
  if (!is.symbol(substitute(table))) {
    # avoid constants
    table <- copy(table)
  }
  if (is_seq(table)) {
    ans <- .Call("Cors",
                 "%between%", x, table[c(1L, length(table))],
                 NULL, NULL, NULL,
                 nThread,
                 PACKAGE = "hutilscpp")
    if (!is.null(ans)) {
      return(raw2lgl(ans))
    }
  }

  ans <- fmatchp(x, table, nomatch = 0L, nThread = nThread, fin = TRUE)
  if (.raw) {
    return(ans)
  }
  raw2lgl(ans)
}

#' @rdname fmatchp
#' @export
fnotinp <- function(x, table, nThread = getOption("hutilscpp.nThread", 1L),
                    .raw = 0L) {
  nThread <- check_omp(nThread)
  if (!is.symbol(substitute(table))) {
    # avoid constants
    table <- copy(table)
  }
  ans <- fmatchp(x, table, nomatch = 0L, nThread = nThread, fin = TRUE)
  if (.raw) {
    return(FLIP(ans))
  }
  raw2lgl(FLIP(ans))

}
FLIP <- function(x) {
  .Call("C_FLIP", x, PACKAGE = "hutilscpp")
}


do_par_in_hash_int <- finp
do_par_in_hash_dbl <- finp

# nocov start
match_last_resort <- function(x, table, nomatch = NA_integer_,
                              nThread = getOption("hutilscpp.nThread", 1L),
                              fin = FALSE,
                              whichFirst = 0L) {
  if (isTRUE(fin)) {
    return(x %in% table)
  }
  if (whichFirst) {
    if (whichFirst == 1L) {
      return(first_which(x %in% table))
    }
    if (whichFirst == -1L) {
      return(last_which(x %in% table))
    }
  }
  match(x, table, nomatch = nomatch)
}
# nocov end
