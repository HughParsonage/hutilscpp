#' Evaluate time of computation
#' @description (Used for examples and tests)
#' @param ... Passed to \code{\link[bench]{system_time}}.
#' @export
#'

bench_system_time <- function(...) {
  if (requireNamespace("bench", quietly = TRUE)) {
    bench::system_time(...)
  }
}
