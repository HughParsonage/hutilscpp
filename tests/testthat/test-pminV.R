test_that("pminV works", {
  y <- sample.int(1e6)
  x <- sample.int(1e6)
  expect_identical(pminV(x, y), base::pmin(x, y))

  xd <- as.double(x)
  yd <- as.double(y)
  expect_identical(pminV(xd, yd), base::pmin(xd, yd))
})
