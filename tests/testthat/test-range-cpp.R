context("test-range-cpp")

test_that("range works", {
  y <- 1
  expect_identical(range(y), range_rcpp(y)[1:2])
  y <- c(1, 1)
  expect_identical(range(y), range_rcpp(y)[1:2])
  y <- as.integer(y)
  expect_identical(range(y), range_rcpp(y)[1:2])
  y <- runif(500)
  expect_identical(range(y), range_rcpp(y)[1:2])
  y <- sample(1:500, size = 450)
  expect_identical(range(y), range_rcpp(y)[1:2])
  y <- seq(0, 100, length.out = 5)
  expect_identical(range(y), range_rcpp(y)[1:2])
  y <- rev(y)
  expect_identical(range(y), range_rcpp(y)[1:2])
  y <- as.integer(y)
  expect_identical(range(y), range_rcpp(y)[1:2])
  y <- rev(y)
  expect_identical(range(y), range_rcpp(y)[1:2])
})

test_that("range falls through", {
  y <- as.character(c(letters, 1:5, letters))
  expect_equal(range_rcpp(y)[1:2], range_rcpp(y))
})

test_that("halting", {
  z <- c(0, 0, -2, 2)
  y <- do_range_dbl(z, -1, 1)
  expect_equal(y[1:2], c(-2, 0))
  expect_equal(y[3], c(3))
  y <- do_range_dbl(z, -3, 1)
  expect_equal(y[4], c(4))

  z <- as.integer(z)
  y <- do_range_int(z, -1L, 1L)
  expect_equal(y[1:2], c(-2, 0))
  expect_equal(y[3], c(3))
  y <- do_range_int(z, -3, 1)
  expect_equal(y[4], c(4))
})

test_that("range on empty", {
  expect_warning(range0 <- range_rcpp(double(0)),
                 regexp = "no non-missing arguments to range_rcpp; returning c(Inf, -Inf).",
                 fixed = TRUE)
  expect_equal(range0, c(Inf, -Inf))
  expect_warning(range_rcpp(integer(0)),
                 regexp = "no non-missing arguments to range_rcpp; returning c(Inf, -Inf).",
                 fixed = TRUE)
  expect_warning(range0i <- range_rcpp(integer(0), integer0_range_is_integer = TRUE),
                 regexp = "no non-missing arguments to range_rcpp; returning c(INT_MAX, -INT_MAX).",
                 fixed = TRUE)
  expect_true(is.integer(range0i))
  expect_true(is.unsorted(range0i))
})

test_that("range on logical", {
  expect_equal(range_rcpp(logical(100)), c(range(logical(100)), 1L, 1L))
  expect_equal(range_rcpp(!logical(100)), c(range(!logical(100)), 1L, 1L))
  expect_equal(range_rcpp(c(FALSE, TRUE, FALSE)), c(0L, 1L, 1L, 2L))
  expect_equal(range_rcpp(c(FALSE, TRUE, NA)), c(0L, 1L, 1L, 2L))
  expect_equal(range_rcpp(c(logical(100), TRUE, NA)), c(0L, 1L, 1L, 101L))
  expect_equal(range_rcpp(c(NA, NA, NA)), as.integer(c(NA, NA, NA, NA)))
})
