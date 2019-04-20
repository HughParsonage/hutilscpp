context("test-or3")

test_that("logical3 works", {
  x <- c(TRUE, FALSE, TRUE)
  y <- logical(3)
  z <- c(TRUE, TRUE, FALSE)
  expect_equal(or3(x, y, z), x | y | z)
  z <- NULL
  expect_equal(or3(x, y, z), x | y | z)
})
