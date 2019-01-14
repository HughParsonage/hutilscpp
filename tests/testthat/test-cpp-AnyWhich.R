context("test-cpp-anywhich")

test_that("AnyWhich error handling", {
  expect_error(AnyWhich(1, 1, gt = TRUE, lt = TRUE, eq = FALSE))
})

test_that("AnyWhich basic", {
  expect_equal(AnyWhich(5:1, 2, gt = FALSE, lt = TRUE, eq = TRUE), 4L)
  expect_equal(AnyWhich(5:1, 2, gt = FALSE, lt = TRUE, eq = FALSE), 5L)
  cov <- 50 + 5:1
  expect_equal(which_first(cov <= 54), 2L)
  expect_equal(which_first(cov < 54), 3L)
})
