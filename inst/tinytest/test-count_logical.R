# test_that("count_logical works", {
expect_error(count_logical(double(1)), "logical")
expect_identical(count_logical(c(TRUE, FALSE, NA)), c(1L, 1L, 1L))
expect_identical(count_logical(c(TRUE, FALSE, TRUE)), c(1L, 2L, 0L))
if (at_home() && !hutilscpp:::is_covr()) {
  if (requireNamespace("parallel", quietly = TRUE)) {
    big_false <- logical(3e9)
    expect_equal(count_logical(big_false), c(3e9, 0, 0))
    expect_equal(count_logical(big_false, parallel::detectCores()), c(3e9, 0, 0))
  }
}


# test_that("count_logical long", {
if ((at_home() || hutilscpp:::is_covr()) && .Machine$sizeof.pointer == 8) {

  x <- logical(.Machine$integer.max + 1)
  cl <- count_logical(x)
  expect_equal(cl, c(.Machine$integer.max + 1, 0, 0))
  cl <- count_logical(x, nThread = 2)
  expect_equal(cl, c(.Machine$integer.max + 1, 0, 0))
  x <- NULL
  x <- hutilscpp:::allocate0_except(.Machine$integer.max + 1, 2, NA_integer_, nThread = 2)
  storage.mode(x) <- "logical"
  cl <- count_logical(x, nThread = 2)
  expect_equal(cl, c(.Machine$integer.max, 0, 1))
}
