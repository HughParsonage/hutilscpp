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

test_that("No in-place by default", {
  x <- rcauchy(100)
  rx <- range(x)
  y <- rcauchy(100)
  z <- rcauchy(100)
  a <- rcauchy(1)
  pmaxC(x, a)
  pminC(x, a)
  pmaxV(x, y)
  pminV(x, y)
  pmax3(x, y, z)
  pmin3(x, y, z)
  expect_identical(range(x), rx)
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
  expect_identical(pmaxC(-5:5 + 0, 0L), pmax.int(-5:5 + 0, 0L))
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
  expect_equal(min(do_pmax0(c(-1, 0, 1))), 0)
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

test_that("pminC error handling", {
  expect_error(pminC("", ""), "character")
  expect_error(pminC(0, ""), "character")
  expect_error(pminC(0, 0:5), "`a` had length")
  expect_identical(pminC(double(0)), double(0))
})

test_that("pminC in-place", {
  o <- c(-1, 0, 1)
  expect_equal(pminC(o, 0.5), c(-1, 0, 0.5))
  expect_equal(o, c(-1, 0, 1))
  pminC(o, 0.5, in_place = TRUE)
  expect_equal(o, c(-1, 0, 0.5))
  expect_equal(pmin0(o), c(-1, 0, 0))
  pmin0(o, in_place = TRUE)
  expect_equal(o, c(-1, 0, 0))
  expect_equal(pmin0(letters), pmin(letters, 0))

  oi <- -1:5
  expect_equal(min(pmax0(oi)), 0L)
  expect_equal(max(pmin0(oi)), 0L)
  expect_equal(max(oi), 5L)
  expect_identical(max(pmin0(oi, TRUE)), 0L)
  expect_equal(max(oi), 0L)
})

test_that("pminV character", {
  o <- letters[10:5]
  o2 <- rep(letters[6], 6)
  sW <- suppressWarnings
  expect_equal(sW({pminV(o, o2, in_place = TRUE)}), pmin(o, o2))
  expect_equal(o, letters[10:5])
})

test_that("pmaxV", {
  expect_identical(pmaxV(1:5, 11:15), 11:15)
  expect_identical(pmaxV(1:5, 5:1), c(5L, 4L, 3L, 4L, 5L))
})

test_that("pmaxV error", {
  expect_error(pmaxV(1:5, 1:5 + 0.5), regexp = "type double")
  expect_error(do_pminV_int(1:5, 1:6), "same length")
  expect_error(do_pminV_dbl(1:5 + 0, 1:6 + 0), "same length")
  expect_error(do_pmaxIntInt(1:5, 1:6), "same length")
  expect_error(do_pmaxNumNum(1:5 + 0, 1:6 + 0), "same length")
})

test_that("benchmark", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("LOGONSERVER"), "\\\\DESKTOP-D6TKKU5"))
  skip_if_not(identical(.Platform$r_arch, "x64"))
  skip_if_not_installed("bench")
  y <- rnorm(5000, 1)
  y <- rep_len(y, 1e9)
  xip <- y + 0
  bench_time_pmaxC_y_0 <- bench::system_time(pmaxC(y, 0))
  expect_lt(as.double(bench_time_pmaxC_y_0[2]), 10) # seconds
  bench_time_pmaxC_y2_0 <- bench::system_time(pmaxC(y, 0, in_place = TRUE))
  expect_lt(as.double(bench_time_pmaxC_y2_0[2]), 4) # seconds
})

test_that("pmax3", {
  x <- c(-1L, 1L, -2L, -5L, -4L, 2L, 3L)
  y <- c(5L, -4L, 1L, 4L, -1L, 0L, -3L)
  z <- c(0L, -2L, -4L, -1L, 3L, 2L, -5L)
  expect_equal(pmax3(x, y, z), pmax(x, pmax(y, z)))
  expect_true(is.integer(pmax3(x, y, z)))
  expect_equal(pmax3(x, 1L, z), pmax(x, pmax(z, 1L)))
  expect_equal(pmax3(x, z, 1L), pmax(x, pmax(z, 1L)))
  expect_error(pmax3(x, z[1:5], z), "length 5")
  expect_error(pmax3(x, z, z[1:4]), "length 4")
  y <- as.double(y)
  expect_equal(pmax3(x, y, z), pmax(x, pmax(y, z)))
  expect_true(is.integer(pmax3(x, y, z)))
  expect_equal(pmax3(x, 1L, z), pmax(x, pmax(z, 1L)))
  expect_equal(pmax3(x, z, 1L), pmax(x, pmax(z, 1L)))
  expect_error(pmax3(x, z[1:5], z), "length 5")
  expect_error(pmax3(x, z, z[1:4]), "length 4")
  z <- as.double(z)
  expect_equal(pmax3(x, y, z), pmax(x, pmax(y, z)))
  expect_true(is.integer(pmax3(x, y, z)))
  expect_equal(pmax3(x, 1L, z), pmax(x, pmax(z, 1L)))
  expect_equal(pmax3(x, z, 1L), pmax(x, pmax(z, 1L)))
  expect_error(pmax3(x, z[1:5], z), "length 5")
  expect_error(pmax3(x, z, z[1:4]), "length 4")
  expect_error(pmax3(2:3, c(2, 2.5), 2:3), regexp = "`y` was type double, but entry 2")
  expect_error(pmax3(2:3, 2:3, c(2, 2.5)), regexp = "`z` was type double, but entry 2")
  expect_error(pmax3("a", "b", "c"), regexp = "must be numeric")

  x <- as.double(x)
  expect_equal(pmax3(x, y, z), pmax(x, pmax(y, z)))
  expect_true(is.double(pmax3(x, y, z)))
  expect_equal(pmax3(x, 1L, z), pmax(x, pmax(z, 1L)))
  expect_equal(pmax3(x, z, 1L), pmax(x, pmax(z, 1L)))
  expect_error(pmax3(x, z[1:5], z), "length 5")
  expect_error(pmax3(x, z, z[1:4]), "length 4")

})

test_that("pmin3", {
  x <- c(-3L, 2L, -1L, 5L, 3L, -5L, -2L)
  y <- c(-4L, -5L, 4L, 2L, 5L, -3L, -1L)
  z <- c(3L, -4L, -2L, 5L, -1L, 4L, 0L)
  expect_equal(pmin3(x, y, z), pmin(x, pmin(y, z)))
  expect_true(is.integer(pmin3(x, y, z)))
  expect_equal(pmin3(x, 1L, z), pmin(x, pmin(z, 1L)))
  expect_equal(pmin3(x, z, 1L), pmin(x, pmin(z, 1L)))
  expect_error(pmin3(x, z[1:5], z), "length 5")
  expect_error(pmin3(x, z, z[1:4]), "length 4")
  y <- as.double(y)
  expect_equal(pmin3(x, y, z), pmin(x, pmin(y, z)))
  expect_true(is.integer(pmin3(x, y, z)))
  expect_equal(pmin3(x, 1L, z), pmin(x, pmin(z, 1L)))
  expect_equal(pmin3(x, z, 1L), pmin(x, pmin(z, 1L)))
  expect_error(pmin3(x, z[1:5], z), "length 5")
  expect_error(pmin3(x, z, z[1:4]), "length 4")
  z <- as.double(z)
  expect_equal(pmin3(x, y, z), pmin(x, pmin(y, z)))
  expect_true(is.integer(pmin3(x, y, z)))
  expect_equal(pmin3(x, 1L, z), pmin(x, pmin(z, 1L)))
  expect_equal(pmin3(x, z, 1L), pmin(x, pmin(z, 1L)))
  expect_error(pmin3(x, z[1:5], z), "length 5")
  expect_error(pmin3(x, z, z[1:4]), "length 4")
  expect_error(pmin3(2:3, c(2, 2.5), 2:3), regexp = "type double, but entry 2")
  expect_error(pmin3(2:3, 2:3, c(0, -0.1)), regexp = "type double, but entry 2")
  expect_error(pmin3("a", "b", "c"), regexp = "must be numeric")

  x <- as.double(x)
  expect_equal(pmin3(x, y, z), pmin(x, pmin(y, z)))
  expect_true(is.double(pmin3(x, y, z)))
  expect_equal(pmin3(x, 1L, z), pmin(x, pmin(z, 1L)))
  expect_equal(pmin3(x, z, 1L), pmin(x, pmin(z, 1L)))
  expect_error(pmin3(x, z[1:5], z), "length 5")
  expect_error(pmin3(x, z, z[1:4]), "length 4")


})

test_that("pmax pure c error", {
  expect_error(.Call("do_c_pmax", 1:5 + 0, 1:2 + 0, 1), regexp = "a did not have length 1")
  expect_error(.Call("do_c_pmax", 1:5 + 0, 1, 1:2 + 0), regexp = "b did not have length 1")
})

test_that("pmaxV x already dominates", {
  w <- c(1L, 5L, 2L, .Machine$integer.max - 2L, 0L)
  expect_equal(pmaxV(w, w - 1L), pmax(w, w - 1L))
  expect_equal(pminV(w, w + 1L), pmin(w, w + 1L))
  w <- as.double(w)
  expect_equal(pmaxV(w, w - 1L), pmax(w, w - 1L))
  expect_equal(pminV(w, w + 1L), pmin(w, w + 1L))
})




