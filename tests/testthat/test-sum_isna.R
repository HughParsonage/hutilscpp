test_that("Early returns", {
  expect_error(sum_isna(list(1:4, 1)))
  expect_equal(sum_isna(NULL), 0)
  expect_equal(sum_isna(integer()), 0)
  expect_equal(sum_isna(1:4, TRUE), 0)
})

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
  skip_if_not(identical(.Platform$r_arch, "x64"))
  do_skip <- FALSE
  tryCatch(x <- logical(1e10), error = function(e) {
    if (grepl("allocate", e$m)) {
      do_skip = TRUE
    } else {
      cat(e$m)
    }
  })
  skip_if(do_skip)
  expect_identical(sum_isna(x), 0L)

  x <- NULL
  x <- rep_len(c(TRUE, NA), 1e10)
  expect_equal(sum_isna(x), 5e9)
})


test_that("sum_isfalse", {
  x <- logical(1e5)
  expect_equal(sum_isfalse(x), 1e5)
  x <- c(TRUE, FALSE, NA)
  expect_equal(sum_isfalse(x), 1)
})
