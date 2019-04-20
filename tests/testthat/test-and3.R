context("test-and3")

test_that("and3 works", {
  x <- c(TRUE, FALSE, TRUE)
  y <- logical(3)
  z <- c(TRUE, TRUE, FALSE)
  expect_equal(and3(x, y, z),
               x & y & z)
})
