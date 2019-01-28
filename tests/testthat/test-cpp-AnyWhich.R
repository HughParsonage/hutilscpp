context("test-cpp-anywhich")

test_that("AnyWhich error handling", {
  expect_error(AnyWhich(1, 1, gt = TRUE, lt = TRUE, eq = FALSE))
})

test_that("AnyWhich basic", {
  expect_equal(AnyWhich_int(5:1, 2L, gt = FALSE, lt = TRUE, eq = TRUE), 4L)
  expect_equal(AnyWhich_dbl(5:1 + 0, 2, gt = FALSE, lt = TRUE, eq = TRUE), 4L)
  expect_equal(AnyWhich_int(5:1, 2L, gt = FALSE, lt = TRUE, eq = FALSE), 5L)
  expect_equal(AnyWhich_dbl(5:1 + 0, 2, gt = FALSE, lt = TRUE, eq = FALSE), 5L)
  cov <- 50 + 5:1
  expect_equal(which_first(cov <= 54), 2L)
  expect_equal(which_first(cov < 54), 3L)
})
