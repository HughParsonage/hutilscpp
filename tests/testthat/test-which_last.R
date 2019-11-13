context("which_last")

test_that("which_last works on integers", {
  x <- c(2L, 1L, 0L, 1L, 2L)
  expect_equal(which_last(x == 2), length(x))
  expect_equal(which_last(x != 2), length(x) - 1L)
  expect_equal(which_last(x == 1), length(x) - 1L)
  expect_equal(which_last(x != 1), length(x))
  expect_equal(which_last(x >= 1), length(x))
  expect_equal(which_last(x <= 1), length(x) - 1L)
  expect_equal(which_last(x > 1), length(x))
  expect_equal(which_last(x < 1), 3L)
})

test_that("which_last works on doubles", {
  x <- as.double(c(2L, 1L, 0L, 1L, 2L))
  expect_equal(which_last(x == 2), length(x))
  expect_equal(which_last(x != 2), length(x) - 1L)
  expect_equal(which_last(x == 1), length(x) - 1L)
  expect_equal(which_last(x != 1), length(x))
  expect_equal(which_last(x >= 1), length(x))
  expect_equal(which_last(x <= 1), length(x) - 1L)
  expect_equal(which_last(x > 1), length(x))
  expect_equal(which_last(x < 1), 3L)
})

test_that("which_last on logicals", {
  x <- c(FALSE, TRUE, FALSE, TRUE)
  expect_equal(which_last(x), 4L)
  expect_equal(which_last(x == TRUE), 4L)
  expect_equal(which_last(x == FALSE), 3L)
  expect_equal(which_last(x != TRUE), 3L)
  expect_equal(which_last(x != FALSE), 4L)
})

