context("test-pmin")

test_that("Error handling", {
  expect_error(pminV(1:5, 1:6),
               regexp = "length")
  expect_error(pminC(1:5, 0L, in_place = "not logical"))
  expect_error(pminC("abc", "abc"),
               regexp = "must be numeric")
})

test_that("pmin's works", {
  y <- sample.int(1e6)
  x <- sample.int(1e6)
  expect_identical(pminV(x, y), base::pmin(x, y))

  xd <- as.double(x)
  yd <- as.double(y)
  expect_identical(pminV(xd, yd), base::pmin(xd, yd))
})

test_that("pminC int", {
  x <- c(1L, -3L, 2L, .Machine$integer.max)
  expect_identical(pminC(x, 3L), pmin.int(x, 3L))
  expect_identical(do_pminC_int(x, 3L), pmin.int(x, 3L))
  y <- c(x, 4L)
  res <- pmin.int(y, 0L)
  expect_identical(pminC(y, 0L, in_place = TRUE), res)
  expect_equal(pminC(y, 0L), y)
  expect_equal(pminC(y, 0), y)


})
