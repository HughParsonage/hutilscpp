#' How wide is the extent of vector?
#' @description Equivalent to \code{diff(minmax(x))}
#'
#' @param x A numeric vector.
#' @param width \code{numeric(1)} (For \code{thinner}, the maximum width)
#' @param nThread Number of threads to use.
#'
#' @return
#' A single value:
#' \describe{
#' \item{\code{extent}}{The difference of \code{minmax(x)}}
#' \item{\code{thinner}}{Equivalent to \code{extent(x) <= width}}
#' }
#'
#' @export
extent <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
  # C version not faster
  diff(minmax(x, nThread = nThread))
}

#' @rdname extent
#' @export
thinner <- function(x, width, nThread = getOption("hutilscpp.nThread", 1L)) {
  return(diff(minmax(ans, nThread = nThread)) <= width)
}

