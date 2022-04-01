#' Most common element
#' @param x An integer vector.
#' @param nThread Number of threads to use.
#' @param .range_fmatch If the range of \code{x} differs by more than
#' this amount, the mode will be calculated via \code{fmatchp}.
#' @param .option \code{integer(1)} Handle exceptional cases:
#' \describe{
#' \item{0}{Returns \code{NULL} quietly.}
#' \item{1}{Returns an error if the mode cannot be calculated.}
#' \item{2}{Emits a warning if the mode cannot be calculate, falls back to \code{hutils::Mode}}
#' }
#'
#' @examples
#' ModeC(c(1L, 1L, 2L))
#'
#'
#' @export


ModeC <- function(x,
                  nThread = getOption("hutilscpp.nThread", 1L),
                  .range_fmatch = 1e9,
                  .option = 1L) {
  xminmax <- minmax(x, nThread = nThread)
  if (anyNA(xminmax) ||
      xminmax[1] + .range_fmatch <= xminmax[2] ||
      !is.integer(x)) {
    fx <- fmatchp(x, x, nThread = nThread)
    mode_fx <- .Call("C_Mode", fx, nThread, NULL, PACKAGE = "hutilscpp")
    return(x[mode_fx])
  }
  ans <- .Call("C_Mode", x, nThread, xminmax, PACKAGE = "hutilscpp")
  if (is.null(ans)) {
    if (.option) {
      switch(.option,
             stop("Unable to calculate ModeC."),
             {
               warning("Unable to calculate ModeC, falling back to hutils::Mode")
               return(hutils::Mode(x))
             })
    }
  }
  ans
}



