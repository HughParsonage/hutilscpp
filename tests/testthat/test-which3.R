test_that("which3 works", {
  expect_equal(which3(c(TRUE, FALSE, FALSE),
                      c(FALSE, TRUE, FALSE),
                      c(FALSE, FALSE, TRUE)),
               integer(0))
  expect_equal(which3(c(TRUE, FALSE, FALSE),
                      c(FALSE, TRUE, FALSE),
                      c(FALSE, FALSE, TRUE),
                      And = FALSE),
               1:3)

  x <- c(TRUE, FALSE, NA)
  expect_equal(which3(x, x, x),
               which(x & x & x))
  y <- c(TRUE, TRUE, TRUE)
  expect_equal(which3(x, y, x),
               which(x & y & x))



})


test_that("long failing instance", {
  skip_on_cran()
  N <- 1e8L
  if (nzchar(Sys.getenv("TRAVIS"))) {
    N <- 1e6L
  }

  x <- rep_len(1:20 %in% c(1, 5, 8, 9), N)
  y <- rep_len(1:10 %in% c(1, 5, 8, 9), N)
  z <- rep_len(TRUE, N)

  expect_identical(which3(x, y, z),
                   which(x & y & z))

  x <- rep_len(1:20 %in% c(2, 5, 8, 9), N)
  y <- rep_len(1:10 %in% c(1, 5, 8, 9), N)
  z <- rep_len(TRUE, N)

  expect_identical(which3(x, y, z),
                   which(x & y & z))

  z <- logical(N)

  expect_identical(which3(x, y, z),
                   which(x & y & z))

})


