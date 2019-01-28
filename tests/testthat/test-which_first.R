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

  x <- c(-2, -1.5)
  expect_identical(which_first(x == -1.5), 2L)
  expect_identical(which_first(x == -2), 1L)
  expect_identical(which_first(x == -2L), 1L)
  expect_identical(which_first(x == 2L), 0L)
  expect_identical(which_first(x == 2), 0L)

  expect_identical(which_first(x >= -1.5), 2L)
  expect_identical(which_first(x >= -2), 1L)
  expect_identical(which_first(x >= -2L), 1L)
  expect_identical(which_first(x >= 2L), 0L)
  expect_identical(which_first(x >= 2), 0L)

  expect_identical(which_first(x > -1.5), 0L)
  expect_identical(which_first(x > -1.6), 2L)
  expect_identical(which_first(x > -2), 2L)
  expect_identical(which_first(x > -2L), 2L)
  expect_identical(which_first(x > 2L), 0L)
  expect_identical(which_first(x > 2), 0L)

  expect_identical(which_first(x <= -1.5), 1L)
  expect_identical(which_first(x <= -2), 1L)
  expect_identical(which_first(x <= -2L), 1L)
  expect_identical(which_first(x <= 2L), 1L)
  expect_identical(which_first(x <= -32L), 0L)
  expect_identical(which_first(x <= -32), 0L)
  expect_identical(which_first(x <= 2), 1L)
  expect_identical(which_first(x <= -2.2), 0L)

  expect_identical(which_first(x < -1.5), 1L)
  expect_identical(which_first(x < -2), 0L)
  expect_identical(which_first(x < -2L), 0L)
  expect_identical(which_first(x < 2L), 1L)
  expect_identical(which_first(x < -1L), 1L)
  expect_identical(which_first(x < -32), 0L)
  expect_identical(which_first(x < 2), 1L)

  expect_identical(which_first(x != -1.5), 1L)
  expect_identical(which_first(x != -2), 2L)
  expect_identical(which_first(x != -2L), 2L)
  expect_identical(which_first(x != 2L), 1L)
  x <- -1
  expect_identical(which_first(x != -1), 0L)
  expect_identical(which_first(x != -1L), 0L)


  y <- -5:5
  y0 <- integer(0)
  yr <- rev(y)
  expect_identical(which_first(y == -4), 2L)
  expect_identical(which_first(y == -4L), 2L)
  expect_identical(which_first(y == -6L), 0L)
  expect_identical(which_first(y == -5.5), 0L)
  expect_identical(which_first(y == -0.01), 0L)
  expect_identical(which_first(y == 5), length(y))
  expect_identical(which_first(y == 5L), length(y))
  expect_identical(which_first(y == 2.5), 0L)

  expect_identical(which_first(y != -4), 1L)
  expect_identical(which_first(y != -5), 2L)
  expect_identical(which_first(y != -5L), 2L)
  expect_identical(which_first(y != 2.5), 1L)
  # corner case
  expect_identical(which_first(y0 != 0), 0L)
  expect_identical(which_first(y0 != 0L), 0L)
  expect_identical(which_first(y0 != 0.5), 0L)


  expect_identical(which_first(y >= 5), length(y))
  expect_identical(which_first(y >= 5L), length(y))
  expect_identical(which_first(y >= 55), 0L)
  expect_identical(which_first(y >= 55L), 0L)
  expect_identical(which_first(y >= -5L), 1L)
  expect_identical(which_first(y >= 4.5), length(y))
  expect_identical(which_first(y >= -4.5), 2L)

  expect_identical(which_first(y <= 5), 1L)
  expect_identical(which_first(y <= 5.5), 1L)
  expect_identical(which_first(y <= 1), 1L)
  expect_identical(which_first(y <= -5.5), 0L)
  expect_identical(which_first(yr <= -5), length(yr))

  expect_identical(which_first(y < 5), 1L)
  expect_identical(which_first(y < 5L), 1L)
  expect_identical(which_first(y < 5.5), 1L)
  expect_identical(which_first(y < -4.5), 1L)
  expect_identical(which_first(y < -5.5), 0L)
  expect_identical(which_first(y < 1), 1L)
  expect_identical(which_first(y < 1L), 1L)
  expect_identical(which_first(y < -11L), 0L)
  expect_identical(which_first(yr < 1.5), 5L)
  expect_identical(which_first(yr < -4), length(yr))

  expect_identical(which_first(y > 0), 7L)
  expect_identical(which_first(y > -1), 6L)
  expect_identical(which_first(y > -2.5),
                   any(y > -2.5) * which.max(y > -2.5))

  y2 <- as.integer(c(0, -1, -2, -1, 0))
  y3 <- as.integer(c(2, 1, 0, 3))
  expect_identical(which_first(y2 <= -2.5),
                   Position(f = function(k) k <= -2.5,
                            x = y2,
                            nomatch = 0L))
  expect_identical(which_first(y3 <= 0.5),
                   Position(f = function(k) k <= 0.5,
                            x = y3,
                            nomatch = 0L))

  expr <- c(0, 5, 3, 2)
  expect_identical(which_first(expr %in% c(22, 32)), 0L)
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
  expect_message(which_first(!y,
                             verbose = TRUE),
                 regexp = "which.max",
                 fixed = TRUE)

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





