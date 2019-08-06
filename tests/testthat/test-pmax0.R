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
  expect_identical(pmax0(x), do_pmax0_radix_sorted(x))
})

test_that("firstNonnegativeRadix", {
  x <- c(-1, -1, 0, 0, 1, 1)
  expect_equal(firstNonNegativeRadix(x) + 1L, 3L)
  x <- rev(x)
  expect_equal(firstNonNegativeRadix(x, desc = TRUE) + 1L, 3L)

  big <- seq(-99e3, 75e4, length.out = 1e4)
  expect_equal(which_first(big >= 0), firstNonNegativeRadix(big) + 1L)
})
