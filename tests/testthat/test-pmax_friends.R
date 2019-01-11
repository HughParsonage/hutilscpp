context("Test pmaxC, pminC etc work as intended")

test_that("grattan functions and pmax pmin give identical results", {
  x <- rcauchy(100)
  y <- rcauchy(100)
  z <- rcauchy(100)
  a <- rcauchy(1)
  expect_equal(pmaxC(x, a), pmax(x, a))
  expect_equal(pminC(x, a), pmin(x, a))
  expect_equal(pmaxV(x, y), pmax(x, y))
  expect_equal(pminV(x, y), pmin(x, y))
  expect_equal(pmax3(x, y, z), pmax(x, pmax(y, z)))

  expect_equal(pmin0(c(-1, 0, 1)),
               pmin(c(-1, 0, 1), 0))

  expect_error(pmax3(1, 2, 3:4))
  expect_error(pmaxV(1, 1:2))
  expect_error(pminV(1, 1:2))
})

test_that("pmaxIPint0", {
  skip_if_not_installed("hutils")
  expect_equal(pmax0(-2:2),
               hutils::if_else(-2:2 > 0, -2:2, 0L))
  expect_equal(pmax0(1:5),
               1:5)
})

test_that("Error handling", {
  expect_error(pmaxC("", ""),
               regexp = "was a character, but must be numeric",
               fixed = TRUE)
  expect_error(pmaxC(0:6, ""),
               regexp = "was a character, but must be numeric",
               fixed = TRUE)
  expect_error(pmaxC(1:5, 1:2),
               "`a` had length 2, but must be length-one",
               fixed = TRUE)
  expect_message(pmaxC(1:5, 0.5),
                 "Output is double.")
})

test_that("pmaxC integer", {
  expect_identical(pmaxC(-5:5, 0L), pmax.int(-5:5, 0L))
  expect_identical(pmaxC(-5:5, 0), pmax.int(-5:5, 0L))
})

test_that("pmaxC corners", {
  expect_identical(pmaxC(integer(0), 0), integer(0))
  expect_identical(pmaxC(double(0), 0), double(0))
})

test_that("pmaxC in-place", {
  o <- c(-1, 0, 1)
  pmaxC(o, 0.5)
  expect_equal(min(o), -1)
  pmaxC(o, 0.5, in_place = TRUE)
  expect_equal(min(o), 0.5)

  i <- 1:5 + 2L
  pmaxC(i, 5L)
  expect_identical(min(i), 3L)
  pmaxC(i, 5L, in_place = TRUE)
  expect_identical(min(i), 5L)
})

test_that("pmax0", {
  expect_identical(pmax0(integer(0)), integer(0))
  expect_equal(pmax0(c(-1, 0.5, 0)), c(0, 0.5, 0))
  expect_error(pmax0(""), regexp = "numeric")
})

test_that("pmax0 in-place", {
  o <- c(-1, 0, 1)
  o0 <- pmax0(o)
  expect_equal(o0[1], 0)
  expect_equal(min(o), -1)
  pmax0(o, in_place = TRUE)
  expect_equal(min(o), 0)

  i <- 1:5 - 2L
  o0 <- pmax0(i)
  expect_equal(o0[1], 0L)
  expect_identical(min(i), -1L)
  pmax0(i, in_place = TRUE)
  expect_identical(min(i), 0L)
})

test_that("benchmark", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("LOGONSERVER"), "\\\\DESKTOP-D6TKKU5"))
  skip_if_not_installed("bench")
  y <- rnorm(5000, 1)
  y <- rep_len(y, 1e9)
  xip <- y + 0
  bench_time_pmaxC_y_0 <- bench::system_time(pmaxC(y, 0))
  expect_lt(as.double(bench_time_pmaxC_y_0[2]), 10) # seconds
  bench_time_pmaxC_y2_0 <- bench::system_time(pmaxC(y, 0, in_place = TRUE))
  expect_lt(as.double(bench_time_pmaxC_y2_0[2]), 4) # seconds
})
