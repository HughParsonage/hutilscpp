# test_that("check_omp works", {
check_omp <- hutilscpp:::check_omp
expect_error(check_omp(NA), "logical")
expect_error(check_omp(NA_integer_), "nThread")
expect_error(check_omp(5.5), "whole number")
expect_equal(check_omp(1), 1)

if (hutilscpp:::has_openmp() && requireNamespace("parallel", quietly = TRUE)) {
  expect_error(check_omp(parallel::detectCores() + 1L), "nThread")
}

