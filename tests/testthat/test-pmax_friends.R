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
