test_that("do_which_in works", {
  x <- 1:10
  y <- 2:5
  expect_equal(do_which_in(x, y), which(x %in% y))
  expect_equal(do_which_in(y, x), which(y %in% x))
})
