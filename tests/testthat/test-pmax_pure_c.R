context("test-pmax_pure_c")

test_that("Pure C works", {
  x <- c(-1, 0, 0.5)
  expect_equal(pmax_pure_c(x, 0), pmax(x, 0))
  expect_equal(pmax_pure_c(x, 0L), pmax(x, 0L))
})
