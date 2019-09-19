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
  expect_identical(do_which_first_int_int(y, x, eq = TRUE), 2L)
})

test_that(".which_first_logical all NA", {
  skip("For later consideration.")
  x <- c(NA, NA, NA)
  expect_equal(.which_first_logical(x, NA), 1)
  expect_equal(.which_first_logical(x, TRUE), 0)
  expect_equal(.which_first_logical(x, FALSE), 0)
})
test_that(".which_first_logical one NA", {
  skip("For later consideration.")
  x <- c(NA, NA, TRUE)
  expect_equal(.which_first_logical(x, NA), 1)
  expect_equal(.which_first_logical(x, TRUE), 3)
  expect_equal(.which_first_logical(x, FALSE), 0)
  y <- c(NA, NA, FALSE)
  expect_equal(.which_first_logical(y, NA), 1)
  expect_equal(.which_first_logical(y, TRUE), 0)
  expect_equal(.which_first_logical(y, FALSE), 3)
  z <- c(TRUE, NA, NA)
  expect_equal(.which_first_logical(z, NA), 2)
  expect_equal(.which_first_logical(z, TRUE), 1)
  expect_equal(.which_first_logical(z, FALSE), 0)
})
test_that(".which_first_logical every", {
  skip("For later consideration.")
  x <- c(NA, TRUE, FALSE)
  expect_equal(.which_first_logical(x, NA), 1)
  expect_equal(.which_first_logical(x, TRUE), 2)
  expect_equal(.which_first_logical(x, FALSE), 3)
  y <- c(FALSE, NA, TRUE)
  expect_equal(.which_first_logical(y, NA), 2)
  expect_equal(.which_first_logical(y, TRUE), 3)
  expect_equal(.which_first_logical(y, FALSE), 1)
})
test_that(".which_first_logical no NA", {
  x <- c(TRUE, TRUE, TRUE)

  expect_equal(.which_first_logical(x, TRUE), 1)
  expect_equal(.which_first_logical(x, FALSE), 0)

  y <- c(FALSE, FALSE, FALSE)

  expect_equal(.which_first_logical(y, TRUE), 0)
  expect_equal(.which_first_logical(y, FALSE), 1)

  z <- c(FALSE, TRUE, FALSE)

  expect_equal(.which_first_logical(z, TRUE), 2)
  expect_equal(.which_first_logical(z, FALSE), 1)
})

test_that("unexpected o", {
  x <- c(NA, TRUE, TRUE)
  expect_equal(which_first(x == TRUE), 2)
  expect_equal(which_first(x == 1L), 2)
  expect_equal(which_first(x == 1), 2)
  expect_equal(which_first(x != 1), 0)

  x.raw <- c(raw(5), charToRaw("A"))
  expect_equal(which_first(x.raw == 0), 1L)
  expect_equal(which_first(x.raw != 0), 6L)
  expect_equal(which_first(x.raw < 0), 0L)
  expect_equal(which_first(x.raw <= 0), 1L)
  expect_equal(which_first(x.raw > 0), 6L)
  expect_equal(which_first(x.raw >= 0), 1L)
  expect_error(which_first(x.raw %in% 0),
               regexp = "not supported")

})

test_that("LHS logical length-one", {
  true <- TRUE
  expect_equal(which_first(true == 1L), 1L)
  expect_equal(which_first(true == 0L), 0L)
  expect_equal(suppressWarnings(which_first(true == NA)), 0L)
  false <- FALSE
  expect_equal(which_first(false == 1), 0L)
  expect_equal(which_first(false == 0), 1L)
  expect_equal(suppressWarnings(which_first(false == NA)), 0L)
  missy <- NA
  expect_equal(which_first(missy == 1), 0L)
  expect_equal(which_first(missy == 1), 0L)
})

test_that("RHS NA", {
  x <- c(NA, NA)
  expect_error(which_first(x > NA),
               regexp = "This is not supported for operator '>'.",
               fixed = TRUE)
  expect_warning(wf_xisna <- which_first(x == NA),
                 regexp = "`rhs` appears to be logical NA.",
                 fixed = TRUE)
  expect_equal(wf_xisna, 1L)
  wf_xisfalse <- which_first(x == 0)
  expect_equal(wf_xisfalse, 0L)

  y <- c(TRUE, FALSE, NA)
  expect_warning(wf_yisna <- which_first(y == NA),
                 regexp = "which_first(is.na",
                 fixed = TRUE)
  expect_equal(wf_yisna, 3L)
  expect_warning(wf_yisntna <- which_first(y != NA),
                 regexp = "which_first(!is.na",
                 fixed = TRUE)
  expect_equal(wf_yisntna, 1L)

  z <- c(NA, FALSE)
  expect_warning(wf_zisntna <- which_first(z != NA),
                 regexp = "`rhs` appears to be logical NA.",
                 fixed = TRUE)
  expect_equal(wf_zisntna, 2L)

})

test_that("lhs_eval length 0", {
  x <- integer(0)
  expect_equal(which_first(x == 0.5), 0L)
  expect_equal(which_first(x != 0.5), 0L)
  expect_equal(which_first(x >= 0.5), 0L)
  expect_equal(which_first(x <= 0.5), 0L)
  expect_equal(which_first(x < 0.5), 0L)
  expect_equal(which_first(x > 0.5), 0L)
  expect_equal(.which_first(c(NA, NA)), 0L)
})


test_that("which_first logical", {
  #10
  x <- y <- c(TRUE, FALSE, NA)
  expect_equal(which_first(x != y), 0)
  expect_equal(which_first(x == y), 1)
  expect_equal(which_first(x > y), 0)
  expect_equal(which_first(x < y), 0)

  y <- as.integer(x)
  expect_equal(which_first(x != y), 0)

  y <- c(2L, 0L, NA)
  expect_equal(which_first(x != y), 1)


})

test_that("which_first_logical %in%", {
  x <- c(TRUE, FALSE, TRUE, FALSE)
  y <- c(TRUE, FALSE)
  expect_equal(which_first(x %in% y), 1)

  x <- rev(x)
  expect_equal(which_first(x %in% y), 1)

  xx <- logical(10)
  yy <- logical(4)
  expect_equal(which_first(xx %in% yy), 1)
  yy <- !yy
  expect_equal(which_first(xx %in% yy), 0)

  yy <- c(TRUE, NA)
  expect_equal(which_first(xx %in% yy), 0)
  xx[3] <- TRUE
  expect_equal(which_first(xx %in% yy), 3)
  xx[3] <- NA
  expect_equal(which_first(xx %in% yy), 3)



  yyy <- TRUE
  expect_equal(which_first(xx %in% yyy), 0)
  yyy <- FALSE
  expect_equal(which_first(xx %in% yyy), 1)
  yyy <- NA
  expect_equal(which_first(xx %in% yyy), 3)

  xxx <- c(TRUE, FALSE)
  yyy <- NA
  expect_equal(which_first(xxx %in% yyy), 0)
  xxx <- c(TRUE, FALSE)
  yyy <- TRUE
  expect_equal(which_first(xxx %in% yyy), 1)
  yyy <- FALSE
  expect_equal(which_first(xxx %in% yyy), 2)

  yyy <- c(TRUE, FALSE, NA)
  expect_equal(which_first(xx %in% yyy), 1)


  a <- c(rep(TRUE, 100), NA)
  b <- c(TRUE, FALSE, TRUE)
  expect_equal(which_first(a %in% b), 1)
  a <- c(rep(FALSE, 100), NA)
  expect_equal(which_first(a %in% b), 1)
  b <- c(TRUE, TRUE)
  expect_equal(which_first(a %in% b), 0)
  b <- c(TRUE, FALSE)
  expect_equal(which_first(a %in% b), 1)
  b <- c(TRUE, NA)
  expect_equal(which_first(a %in% b), 101)

  a <- c(TRUE, TRUE, rep(NA, 100), FALSE, TRUE)
  b <- c(FALSE, NA)
  expect_equal(which_first(a %in% b), 3)
  b <- c(TRUE, NA)
  expect_equal(which_first(a %in% b), 1)

})




test_that("Error handling", {
  x <- c(TRUE, FALSE)
  y <- c(TRUE, FALSE, TRUE)
  expect_error(which_first(x == y), "length")
})

test_that("do_which_first_lgl_lgl", {
  skip_if_not_installed("data.table")
  library(data.table)

  f <- do_which_first_lgl_lgl

  expect_error(f(c(TRUE, FALSE), c(NA), FALSE, FALSE, FALSE), "lengths")
                                                         # eq   # lt   # gt
  expect_equal(f(c(NA, TRUE, FALSE), c(TRUE, FALSE, NA), FALSE, FALSE, FALSE), 1)
  expect_equal(f(c(NA, TRUE, FALSE), c(TRUE, FALSE, NA), FALSE, FALSE, TRUE ), 2)
  expect_equal(f(c(NA, TRUE, FALSE), c(TRUE, FALSE, NA), FALSE, TRUE , FALSE), 0)
  expect_equal(f(c(NA, TRUE, FALSE), c(TRUE, FALSE, NA), TRUE , FALSE, FALSE), 0)
  expect_equal(f(c(NA, TRUE, FALSE), c(TRUE, FALSE, NA), TRUE , FALSE, TRUE ), 1)
  expect_equal(f(c(NA, TRUE, FALSE), c(TRUE, FALSE, NA), TRUE , TRUE , FALSE), 1)

  x <- c(logical(5), TRUE, NA, FALSE)
  y <- c(logical(4), TRUE, TRUE, FALSE, FALSE)
                      # eq   # lt   # gt
  expect_equal(f(x, y, FALSE, FALSE, FALSE), 5)
  expect_equal(f(x, y, TRUE , FALSE, FALSE), 1)
  expect_equal(f(x, y, TRUE , TRUE , FALSE), 1)
  expect_equal(f(x, y, FALSE, TRUE , FALSE), 5)
  expect_equal(f(NA, NA, FALSE, FALSE, FALSE), 0)
  expect_equal(f(NA, NA, TRUE, FALSE, TRUE , skip_na = FALSE), 1)
  expect_equal(f(NA, NA, TRUE, TRUE , FALSE, skip_na = FALSE), 1)
  expect_equal(f(FALSE, NA, TRUE, TRUE, FALSE, skip_na = FALSE), 1)
  expect_equal(f(FALSE, NA, TRUE, FALSE, TRUE, skip_na = FALSE), 0)
  expect_equal(f(FALSE, NA, TRUE, FALSE, TRUE, skip_na = TRUE), 0)
  expect_equal(f(FALSE, FALSE, TRUE, FALSE, TRUE), 1)

})

test_that("do_which_first_lgl_lgl skip_na", {
  x <- c(NA, NA, TRUE, TRUE, FALSE, FALSE)
  y <- c(NA, TRUE, NA, FALSE, NA, TRUE)
  f <- do_which_first_lgl_lgl
  expect_equal(f(x, y, FALSE, FALSE, FALSE, skip_na = TRUE), 4)
  expect_equal(f(x, y, TRUE , FALSE, FALSE, skip_na = TRUE), 0)
  expect_equal(f(x, y, FALSE, FALSE, TRUE , skip_na = TRUE), 4)
  expect_equal(f(x, y, FALSE, TRUE , FALSE, skip_na = TRUE), 6)
  expect_equal(f(x, y, TRUE , TRUE , FALSE, skip_na = TRUE), 6)
  expect_equal(f(x, y, TRUE , TRUE , TRUE, skip_na = TRUE), 4)
})




