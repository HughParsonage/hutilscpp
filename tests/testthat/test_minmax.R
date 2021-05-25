test_that("minmax", {
  x <- sample(100)
  expect_equal(minmax(x), c(1, 100))
  x <- as.double(x)
  expect_equal(minmax(x), c(1, 100))
  x <- letters
  expect_equal(minmax(x), c("a", "z"))
})

