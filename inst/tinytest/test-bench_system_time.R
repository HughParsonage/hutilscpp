#context "test-bench_system_time")

# test_that("bench_system_time covg", {

  # skip_if_not_installed("bench")
  # skip_on_cran()
if ((at_home() || hutilscpp:::is_covr()) && requireNamespace("bench", quietly = TRUE)) {
  expect_true(TRUE) # for valgrind

  expr <- x <- rep_len(c(0, -0.5, 1.2), 5e6)
  bench_x <- bench_system_time(pminC(x, 0))
  expect_true(as.numeric(bench_x)[2] < 1.5)
}


