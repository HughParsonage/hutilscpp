context("which_last")

test_that("which_last works on integers", {
  x <- c(2L, 1L, 0L, 1L, 2L)
  expect_equal(which_last(x == 2), length(x))
  expect_equal(which_last(x != 2), length(x) - 1L)
  expect_equal(which_last(x == 1), length(x) - 1L)
  expect_equal(which_last(x != 1), length(x))
  expect_equal(which_last(x >= 1), length(x))
  expect_equal(which_last(x <= 1), length(x) - 1L)
  expect_equal(which_last(x > 1), length(x))
  expect_equal(which_last(x < 1), 3L)
})

test_that("which_last works on doubles", {
  x <- as.double(c(2L, 1L, 0L, 1L, 2L))
  expect_equal(which_last(x == 2), length(x))
  expect_equal(which_last(x != 2), length(x) - 1L)
  expect_equal(which_last(x == 1), length(x) - 1L)
  expect_equal(which_last(x != 1), length(x))
  expect_equal(which_last(x >= 1), length(x))
  expect_equal(which_last(x <= 1), length(x) - 1L)
  expect_equal(which_last(x > 1), length(x))
  expect_equal(which_last(x < 1), 3L)
})

test_that("which_last on logicals", {
  x <- c(FALSE, TRUE, FALSE, TRUE)
  current_opt <- getOption("hutilscpp_suppressWarning")
  skip_if_not(is.null(current_opt))
  withr::with_options(list(hutilscpp_suppressWarning = FALSE),
                      {
                        expect_warning(which_last(x == NA),
                                       regexp = "appears to be logical NA")
                      })

  options(hutilscpp_suppressWarning = TRUE)

  expect_equal(which_last(x), 4L)
  expect_equal(which_last(x == TRUE), 4L)
  expect_equal(which_last(x == FALSE), 3L)
  expect_equal(which_last(x != TRUE), 3L)
  expect_equal(which_last(x != FALSE), 4L)
  expect_equal(which_last(x %in% c(TRUE, FALSE)), length(x))
  expect_equal(which_last(x %in% c(NA)), 0L)
  expect_equal(which_last(x %in% c(NA, FALSE)), 3L)
  expect_equal(which_last(x == NA), 0L)
  expect_equal(which_last(x != NA), length(x))

  axna <- c(FALSE, TRUE, FALSE, NA, FALSE)
  expect_equal(which_last(axna == TRUE), 2L)
  expect_equal(which_last(axna == FALSE), length(axna))
  expect_equal(which_last(axna == NA), length(axna) - 1L)
  expect_equal(which_last(axna != NA), length(axna))
  expect_equal(which_last(axna != TRUE), length(axna))
  options(hutilscpp_suppressWarning = NULL)
})

test_that("which_last is.na(lhs_eval)", {
  x <- c(TRUE, TRUE, FALSE)
  expect_equal(which_last(x == NA, suppressWarning = TRUE), 0L)
  expect_equal(which_last(x %in% c(NA, TRUE)), 2L)

  xna <- c(TRUE, FALSE, NA, TRUE, FALSE, NA)
  expect_equal(which_last(xna == NA, suppressWarning = TRUE), length(xna))
  expect_equal(which_last(xna != NA, suppressWarning = TRUE), length(xna) - 1L)
  expect_equal(which_last(xna != TRUE), length(xna) - 1L)
  expect_equal(which_last(xna == TRUE), 4L)
  expect_equal(which_last(xna %in% NA), length(xna))
  expect_equal(which_last(xna != FALSE, reverse = TRUE), 1L)
})

