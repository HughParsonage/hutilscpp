#' Number of missing values
#' @description The count of missing values in an atomic vector, equivalent to
#' to \code{sum(is.na(x))}.
#' @param x An atomic vector.
#' @param do_anyNA Should \code{anyNA(x)} be executed before an attempt to
#' count the \code{NA}'s in \code{x} one-by-one? By default, set to \code{TRUE},
#' since it is generally quicker. It will only be slower when \code{NA} is rare
#' and occurs late in \code{x}.
#'
#' Ignored silently if \code{nThread != 1}.
#'
#' @param nThread \describe{
#' \item{\code{nThread}}{Number of threads to use.}
#' }
#'
#' @examples
#' sum_isna(c(1:5, NA))
#' sum_isna(c(NaN, NA))  # 2 from v0.4.0 (Sep 2020)
#' @export

sum_isna <- function(x, do_anyNA = TRUE, nThread = getOption("hutilscpp.nThread", 1L)) {
  if (length(x) == 0L) {
    return(0L)
  }
  if (!is.atomic(x)) {
    stop("`x` was class ", paste0(class(x), collapse = " "), ", but must be atomic.")
  }

  nThread <- check_omp(nThread)

  if (do_anyNA && nThread == 1L && !anyNA(x)) {
    return(0L)
  }
  if (is_altrep(x)) {
    return(as.integer(anyNA(x)))
  }

  .Call("Csum_isna", x, nThread, PACKAGE = packageName)
}



