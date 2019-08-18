test_that("pmax0 abs", {
  x <- c(-1, 0, 1, -1)
  expect_identical(pmax.int(x, 0), do_pmax0_abs_dbl(x))
  # Need to test extrema
  skip_if_not(is.integer(-.Machine$integer.max))
  y <- c(-.Machine$integer.max, .Machine$integer.max, 0L)
  expect_identical(do_pmax0_abs_int(y), c(0L, .Machine$integer.max, 0L))
})

test_that("pmax0 radix", {
  x <- as.double(seq(-1e6, 1e7, length.out = 3e3))
  expect_identical(pmax0(x), do_pmax0_radix_sorted_dbl(x))
  x <- rev(x)
  expect_identical(pmax0(x), do_pmax0_radix_sorted_dbl(x))
  x <- rev(x)
  x <- as.integer(x)
  expect_identical(pmax0(x), do_pmax0_radix_sorted_int(x))
  x <- rev(x)
  expect_identical(pmax0(x), do_pmax0_radix_sorted_int(x))
})

test_that("pmax0 radix extrema", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(identical(.Platform$r_arch, "x64"))
  min_int <- -.Machine$integer.max
  max_int <- +.Machine$integer.max
  x <- min_int:max_int
  res <- do_pmax0_radix_sorted_int(x)
  expect_equal(which_first(x > 0), which_first(res > 0))
  res <- NULL
  x <- NULL
  x <- max_int:min_int
  res <- do_pmax0_radix_sorted_int(x)
  expect_equal(which_first(x == 0), which_first(res == 0))
})

test_that("firstNonnegativeRadix", {
  x <- c(-1, -1, 0, 0, 1, 1)
  expect_equal(firstNonNegativeRadix(x) + 1L, 3L)
  x <- rev(x)
  expect_equal(firstNonNegativeRadix(x, desc = TRUE) + 1L, 3L)

  big <- seq(-99e3, 75e4, length.out = 1e4)
  expect_equal(which_first(big >= 0), firstNonNegativeRadix(big) + 1L)
})

test_that("firstNonnegativeRadix desc", {
  x <- 10:-1
  expect_equal(firstNonNegativeRadix(x, desc = TRUE) + 1L, which_first(x <= 0))
})

test_that("firstNonnegativeRadix corners", {
  x <- 1:10
  expect_lte(firstNonNegativeRadix(x), 1)
  expect_lte(firstNonNegativeRadix(x, mini = 2), 2)
  expect_lte(firstNonNegativeRadix(-x, desc = TRUE), 1)
  expect_lte(firstNonNegativeRadix(-x, desc = TRUE, maxi = 5), 5)
  x <- as.double(x)
  expect_lte(firstNonNegativeRadix(x), 1)
  expect_lte(firstNonNegativeRadix(x, mini = 2), 2)
  expect_lte(firstNonNegativeRadix(-x, desc = TRUE), 1)
  expect_lte(firstNonNegativeRadix(-x, maxi = 5), 5)

  expect_equal(do_firstNonNegativeRadix_int(1:5, mini = -2L), 0)
  expect_equal(do_firstNonNegativeRadix_dbl(1:5, mini = -2L), 0)

})

test_that("Already nonnegative", {
  x <- 1:100
  expect_equal(pmax0(x, sorted = TRUE), pmax(x, 0))
  expect_equal(do_pmax0_abs_int(x), 1:100)
  expect_equal(do_pmax0_radix_sorted_int(x), 1:100)
  x <- as.double(x)
  expect_equal(pmax0(x, sorted = TRUE), 1:100)
  expect_equal(do_pmax0_abs_dbl(x), 1:100)
  expect_equal(do_pmax0_radix_sorted_dbl(x), 1:100)
  x <- x - 2L
  expect_equal(do_pmax0_abs_dbl(x), pmax(x, 0))
  expect_equal(do_pmax0_radix_sorted_dbl(x), pmax(x, 0))
  x <- as.integer(x)
  expect_equal(pmax0(x, sorted = TRUE), pmax(x, 0))
  expect_equal(do_pmax0_abs_int(x), pmax(x, 0))
  expect_equal(do_pmax0_radix_sorted_int(x), pmax(x, 0))
  x <- rev(x)
  expect_equal(pmax0(x, sorted = TRUE), pmax(x, 0))
  expect_equal(do_pmax0_abs_int(x), pmax(x, 0))
  expect_equal(do_pmax0_radix_sorted_int(x), pmax(x, 0))
  x <- as.double(x)
  expect_equal(pmax0(x, sorted = TRUE), pmax(x, 0))
  expect_equal(do_pmax0_abs_dbl(x), pmax(x, 0))
  expect_equal(do_pmax0_radix_sorted_dbl(x), pmax(x, 0))


  x <- c(x, -1L, x)
  expect_equal(do_pmax0_abs_int(x), pmax(x, 0L))
  xd <- as.double(x)
  expect_equal(do_pmax0_abs_dbl(x), pmax(xd, 0))
})

test_that("do_pmin0s", {
  x <- c(0L, 6L, -4L, -2L, -1L, 7L, 9L, 4L, 8L, 3L, 10L, 5L, -3L, 1L, 2L)
  expect_equal(do_pmin0_abs_int(x), pmin(x, 0L))
  x <- sort(x)
  expect_equal(do_pmin0_radix_sorted_int(x), pmin(x, 0L))

  x <- rev(x)

  expect_equal(do_pmin0_abs_int(x), pmin(x, 0L))
  x <- sort(x)
  expect_equal(do_pmin0_radix_sorted_int(x), pmin(x, 0L))


  x <- as.double(x)

  expect_equal(do_pmin0_abs_dbl(x), pmin(x, 0L))
  x <- sort(x)
  expect_equal(do_pmin0_radix_sorted_dbl(x), pmin(x, 0L))

  x <- rev(x)

  expect_equal(do_pmin0_abs_dbl(x), pmin(x, 0L))
  x <- sort(x)
  expect_equal(do_pmin0_radix_sorted_dbl(x), pmin(x, 0L))



  x <- pmin0(x)

  # Now check already nonpositive
  x <- as.double(x)
  expect_equal(do_pmin0_abs_dbl(x), pmin(x, 0L))
  x <- sort(x)
  expect_equal(do_pmin0_radix_sorted_dbl(x), pmin(x, 0L))

  x <- as.integer(x)
  expect_equal(do_pmin0_abs_int(x), pmin(x, 0L))
  x <- sort(x)
  expect_equal(do_pmin0_radix_sorted_int(x), pmin(x, 0L))

  # Now strictly positive
  x <- x - 1L
  expect_equal(do_pmin0_abs_int(x), pmin(x, 0L))
  x <- sort(x)
  expect_equal(do_pmin0_radix_sorted_int(x), pmin(x, 0L))

  x <- as.double(x)
  expect_equal(do_pmin0_abs_dbl(x), pmin(x, 0L))
  x <- sort(x)
  expect_equal(do_pmin0_radix_sorted_dbl(x), pmin(x, 0L))

  x <- c(2, 1, 0, -1, -2)
  expect_equal(do_pmin0_radix_sorted_dbl(x), pmin(x, 0))
  x <- as.integer(x)
  expect_equal(do_pmin0_radix_sorted_int(x), pmin(x, 0))
})

