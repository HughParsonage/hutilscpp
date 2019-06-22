test_that("sum_isna works", {
  x <- logical(0)
  expect_equal(sum(is.na(x)), sum_isna(x))

  x <- c(x, TRUE)
  expect_equal(sum(is.na(x)), sum_isna(x))

  x <- c(x, FALSE)
  expect_equal(sum(is.na(x)), sum_isna(x))

  x <- c(x, NA)
  expect_equal(sum(is.na(x)), sum_isna(x))

  x <- as.integer(x)
  expect_equal(sum(is.na(x)), sum_isna(x))

  x <- as.double(x)
  expect_equal(sum(is.na(x)), sum_isna(x))

  x <- as.complex(x)
  expect_equal(sum(is.na(x)), sum_isna(x))

  x <- as.character(x)
  expect_equal(sum(is.na(x)), sum_isna(x))
})

test_that("sum_isna long", {
  skip_on_cran()
  skip_on_travis()
  x <- logical(1e10)
  expect_equal(sum_isna(x), sum(is.na(x)))
  x <- NULL
  x <- rep_len(c(TRUE, NA), 1e10)
  expect_equal(sum_isna(x), sum(is.na(x)))
})
