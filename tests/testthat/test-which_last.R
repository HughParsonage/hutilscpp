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


  noTRUES <- logical(10)
  expect_equal(which_last(noTRUES == TRUE), 0L)
})

test_that("which_last(x == y)", {
  x <- c(5:1, 0:4)
  y <- c(5:1, 0:4)
  w <- y + 1L
  expect_equal(which_last(x == y), length(x))
  expect_equal(which_last(x == w), 0L)
  expect_equal(which_last(x != y), 0L)
  expect_equal(which_last(x != w), length(x))
  z <- c(5:2, 0:5)
  expect_equal(which_last(x == z), 4L)
  expect_equal(which_last(x != z), length(x))
  expect_equal(which_last(x > z), 5L)

  xd <- as.double(x)
  yd <- as.double(y)
  wd <- as.double(w)
  expect_equal(which_last(xd == yd), length(x))
  expect_equal(which_last(xd == wd), 0L)
  expect_equal(which_last(xd != yd), 0L)
  expect_equal(which_last(xd != wd), length(x))
  zd <- as.double(z)
  expect_equal(which_last(xd == zd), 4L)
  expect_equal(which_last(xd != zd), length(x))
  expect_equal(which_last(xd > zd), 5L)

  i <- c(0.5, 0.4, 0.3, 0.5)
  j <- c(0.5, 0.4, 0.2 + 0.1, 0)
  expect_equal(which_last(i == j), 2)
})

test_that("which_last(all FALSE)", {
  x <- logical(10)
  expect_equal(which_last(x), 0L)
  expect_equal(which_last(x != FALSE), 0L)
  allTRUE <- rep(TRUE, 8)
  expect_equal(which_last(allTRUE != TRUE), 0L)
})

test_that("which_first long", {
  skip_on_travis()
  skip_on_appveyor()
  skip_if(.Machine$sizeof.pointer != 8)
  x <-
    tryCatch(allocate0_except(25e8, c(2e9, 23e8), c(1L, -1L)),
             error = function(e) {
               NULL
             })
  skip_if(is.null(x), message = "error during allocation")
  expect_equal(which_first(x > 2L), 0L)
  expect_equal(which_last(x == 0), length(x))
  expect_equal(which_first(x == 1), 2e9)
  expect_equal(which_first(x < 0), 23e8)
  expect_equal(which_first(x < -0.5), 23e8)
})

test_that("which_last internals", {
  expect_equal(do_which_last_notTRUE(c(FALSE)), 1L)
  expect_equal(do_which_last_notTRUE(c(TRUE)), 0L)
  expect_equal(do_which_last_notTRUE(c(TRUE, TRUE)), 0L)
  expect_equal(do_which_last_notTRUE(c(NA)), 1L)
  expect_equal(do_which_last_notFALSE(c(FALSE)), 0L)
  expect_equal(do_which_last_notFALSE(c(FALSE, FALSE)), 0L)
  expect_equal(do_which_last_notFALSE(c(TRUE)), 1L)
  expect_equal(do_which_last_notFALSE(c(NA)), 1L)
})

test_that("which_last( %between% )", {
  x <- c(1:10, -1L)
  expect_equal(which_last(x %between% c(1L, 10L)), 10L)
  expect_equal(which_last(x %]between[% c(1L, 10L)), 11L)
  x <- as.double(x)
  expect_equal(which_last(x %between% c(1L, 10L)), 10L)
  expect_equal(which_last(x %]between[% c(1L, 10L)), 11L)

  x <- c(1:10, -1L)
  expect_equal(which_last(x %between% c(1, 10)), 10L)
  expect_equal(which_last(x %]between[% c(1, 10)), 11L)
  x <- as.double(x)
  expect_equal(which_last(x %between% c(1, 10)), 10L)
  expect_equal(which_last(x %]between[% c(1, 10)), 11L)
})

test_that("last(lgl lgl)", {
  x <- c(TRUE, FALSE, TRUE)
  y <- c(FALSE, FALSE, TRUE)
  expect_equal(which_last(x == y), 3)
  expect_equal(which_last(x != y), 1)
  expect_equal(which_last(x >= y), 3)
  expect_equal(which_last(x <= y), 3)
  expect_equal(which_last(x > y), 1)
  expect_equal(which_last(x < y), 0)
})
