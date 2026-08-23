#' Power-of-two bounds
#'
#' Find the largest power of two no greater than every element of `x`, and
#' the smallest power of two no less than every element of `x`.
#'
#' @param x A non-empty numeric vector containing only positive, finite values.
#' @param nThread Maximum number of threads to use. `bminmax()` may use fewer
#'   threads when `x` is too short for parallel execution to be beneficial.
#'
#' @return An unnamed double vector of length two. The first element is the
#'   power-of-two lower bound and the second is the power-of-two upper bound.
#'   The upper bound is `Inf` when the mathematical result is `2^1024`, which
#'   is outside the finite double range.
#'
#' @details
#' `bminmax()` makes one pass over `x`. On supported x86 CPUs, sufficiently
#' long vectors use an AVX-512F implementation selected at runtime; other
#' systems use a portable implementation. When OpenMP is available and
#' `nThread > 1`, long vectors are divided into large contiguous chunks. The
#' AVX-512 and OpenMP decisions are independent, and both have serial fallbacks.
#'
#' @examples
#' bminmax(c(3, 9))
#' bminmax(c(0.25, 8))
#'
#' @export
bminmax <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
  .Call(Cbminmax, x, check_omp(nThread))
}

.bminmax_portable <- function(x) {
  .Call(Cbminmax_portable, x)
}

.bminmax_has_avx512 <- function() {
  .Call(Cbminmax_has_avx512)
}
