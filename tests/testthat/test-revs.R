test_that("rev_dbl works", {
  x <- c(NaN, 0, 1, 1, NA)
  y <- do_rev_dbl(x)
  expect_equal(y, rev(x))
  expect_equal(do_rev_dbl(double(0)), rev(double(0)))
})

test_that("rev_dbl works longer", {
  skip_on_cran()
  skip_on_travis()
  x <- do_sparse_dbl(22e8, 2, 1)
  yh <- double(6)
  yt <- c(0, 0, 0, 0, 1, 0)
  zt <- tail(do_rev_dbl(x))
  expect_identical(yt, zt)
})

test_that("rev_int works", {
  x <- c(NA, 1L, 9L)
  expect_equal(rev(x), do_rev_int(x))
})

test_that("rev_int long", {
  skip_on_cran()
  skip_on_travis()
  x <- -2e9:2e9
  expect_true(do_rev_int(x)[1] > 0)
})

