# test_that("count_logical works", {
library(hutilscpp)
expect_error(count_logical(double(1)), "logical")
expect_identical(count_logical(c(TRUE, FALSE, NA)), c(1L, 1L, 1L))
expect_identical(count_logical(c(TRUE, FALSE, TRUE)), c(1L, 2L, 0L))
if (at_home() && !hutilscpp:::is_covr()) {
  if (requireNamespace("parallel", quietly = TRUE)) {
    big_false <- logical(3e4)
    expect_equal(count_logical(big_false), c(3e4, 0, 0))
    expect_equal(count_logical(big_false, parallel::detectCores()), c(3e4, 0, 0))
  }
}


