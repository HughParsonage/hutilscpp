#' What is the diameter of set of points?
#' @description Equivalent to \code{diff(minmax(x))}
#'
#' @param x A numeric vector.
#' @param width \code{numeric(1)} (For \code{thinner}, the maximum width)
#' @param nThread Number of threads to use.
#'
#' @return
#' A single value:
#' \describe{
#' \item{\code{diam}}{The difference of \code{minmax(x)}}
#' \item{\code{thinner}}{Equivalent to \code{diam(x) <= width}}
#' }
#'
#' @export
diam <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
  # C version not faster
  ans <- minmax(x, nThread = nThread)
  if (is.integer(x) && ans[1] < 0) {
    out <- as.double(ans[2]) - ans[1]
    return(R_xlen_t(out))
  }
  ans[2] - ans[1]
}

#' @rdname diam
#' @export
thinner <- function(x, width, nThread = getOption("hutilscpp.nThread", 1L)) {
  diam(x, nThread = nThread) <= width
}

