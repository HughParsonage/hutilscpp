#' Most common element
#' @param x An integer vector.
#' @param nThread Number of threads to use.
#' @param .range_fmatch If the range of \code{x} differs by more than
#' this amount, the mode will be calculated via \code{fmatchp}.
#'
#' @export


ModeC <- function(x,
                  nThread = getOption("hutilscpp.nThread", 1L),
                  .range_fmatch = 1e9) {
  xminmax <- minmax(x, nThread = nThread)
  if (anyNA(xminmax) ||
      xminmax[1] + .range_fmatch <= xminmax[2] ||
      !is.integer(x)) {
    fx <- fmatchp(x, x, nThread = nThread)
    mode_fx <- .Call("C_Mode", fx, nThread, PACKAGE = "hutilscpp")
    return(x[mode_fx])
  }
  .Call("C_Mode", x, nThread, PACKAGE = "hutilscpp")
}

unique_fmatch <- function(x, nThread = 1L) {
  fmatchx <- fmatchp(x, x, nThread = nThread, nomatch = 0L)
  .Call("Cunique_fmatch", x, fmatchx, nThread, PACKAGE = "hutilscpp")
}

