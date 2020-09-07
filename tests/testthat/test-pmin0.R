test_that("pmin0 works", {
  o <- c(-1, 0, 1)
  expect_equal(pmin0(o), c(-1, 0, 0))
  pmin0(o, in_place = TRUE)
  expect_equal(o, c(-1, 0, 0))

  oi <- -1:5
  expect_equal(max(pmin0(oi)), 0L)
  expect_equal(max(oi), 5L)
  expect_identical(max(pmin0(oi, TRUE)), 0L)
  expect_equal(max(oi), 0L)
})

test_that("pmin0 sorted", {
  x <- as.double(seq(-1e6, 1e7, length.out = 3e3))
  expect_identical(pmin0(x), do_pmin0_radix_sorted_dbl(x))
  x <- rev(x)
  expect_identical(pmin0(x), do_pmin0_radix_sorted_dbl(x))
  x <- rev(x)
  x <- as.integer(x)
  expect_identical(pmin0(x), do_pmin0_radix_sorted_int(x))
  x <- rev(x)
  expect_identical(pmin0(x), do_pmin0_radix_sorted_int(x))

  x <- -5:6
  pmin0(x, sorted = TRUE, in_place = TRUE)
  expect_equal(x, pmin(-5:6, 0))
  x <- as.double(-5:6)
  pmin0(x, sorted = TRUE, in_place = TRUE)
  expect_equal(x, pmin(-5:6, 0))
  x <- 5:-6
  pmin0(x, sorted = TRUE, in_place = TRUE)
  expect_equal(x, pmin(5:-6, 0))
  x <- 5:-6 + 0
  pmin0(x, sorted = TRUE, in_place = TRUE)
  expect_equal(x, pmin(5:-6, 0))
})

