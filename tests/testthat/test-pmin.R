context("test-pmin")

test_that("Error handling", {
  expect_error(pminV(1:5, 1:6),
               regexp = "length")
})

test_that("pmin's works", {
  y <- sample.int(1e6)
  x <- sample.int(1e6)
  expect_identical(pminV(x, y), base::pmin(x, y))

  xd <- as.double(x)
  yd <- as.double(y)
  expect_identical(pminV(xd, yd), base::pmin(xd, yd))
})
