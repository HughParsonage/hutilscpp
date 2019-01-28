context("test-which_first")

test_that("which_first works", {
  x <- runif(10, 1, 2)
  expr <- c(0, 5, 3, 2)
  expect_identical(which_first(x < 0), 0L)
  expect_identical(which_first(x > 0), 1L)
  expect_identical(which_first(x == 0), 0L)
  expect_identical(which_first(x >= 0), 1L)
  expect_identical(which_first(x <= 0), 0L)
  expect_identical(which_first(x != 0), 1L)
  expect_identical(which_first(x != 0), 1L)

  expect_identical(which_first(expr %in% c(2, 3)), 3L)
  expr <- as.integer(expr)
  expect_identical(which_first(expr %in% c(2, 3)), 3L)
  expect_identical(which_first(expr %in% c(7, 9)), 0L)
  expect_identical(which_first(expr %in% c(2L, 3L)), 3L)
  expect_identical(which_first(expr %in% c(8L, 9L)), 0L)
})

test_that("Conflicts with expressions", {
  expr <- c(0, 5, 3, 2)
  expect_identical(which_first(expr == 5), 2L)
  lhs <- rhs <- 3
  expect_identical(which_first(expr == lhs), 3L)
})

test_that("Fall through", {
  y <- logical(5)
  expect_identical(which_first(y), 0L)
  expect_identical(which_first(!y), 1L)

  x <- c(letters, 1)
  expect_identical(which_first(x == 1), 27L)
  expect_identical(which_first(x == 155), 0L)
  expect_identical(which_first(x < 1), 0L)
})

test_that("match", {
  int_m <- 1:100
  expect_identical(which_first(int_m == 2L), 2L)
})

test_that("which_first_int_int", {
  x <- sample.int(10, size = 1000, replace = TRUE)
  y <- sample.int(10, size = 1000, replace = TRUE)
  expect_error(do_which_first_int_int(1:9, 1:10), "lengths")
  expect_identical(do_which_first_int_int(x, y), which.max(x == y))
  expect_identical(do_which_first_int_int(x, y, FALSE), which.max(x != y))
  expect_identical(do_which_first_int_int(x, y, TRUE, gt = TRUE), which.max(x >= y))
  expect_identical(do_which_first_int_int(x, x + 1L, TRUE, gt = TRUE), 0L)
  expect_identical(do_which_first_int_int(x, y, TRUE, lt = TRUE), which.max(x <= y))
  expect_identical(do_which_first_int_int(x, y, FALSE, gt = TRUE), which.max(x > y))
  expect_identical(do_which_first_int_int(x, x, FALSE, gt = TRUE), 0L)
  expect_identical(do_which_first_int_int(x, y, FALSE, lt = TRUE), which.max(x < y))
  x <- c(0L, 1L)
  y <- c(0L, 1L)
  expect_identical(do_which_first_int_int(x, y, eq = FALSE), 0L)
  y <- c(2L, 1L)
  expect_identical(do_which_first_int_int(y, x, eq = TRUE, lt = TRUE), 2L)
})





