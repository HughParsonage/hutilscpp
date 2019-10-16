context("test-as_integer_if_safe")

test_that("as_integer_if_safe works", {
  expect_identical(as_integer_if_safe(4), 4L)
  expect_identical(as_integer_if_safe(4:5), 4:5)
  expect_identical(as_integer_if_safe(4:5 + 0), 4:5)
  expect_identical(as_integer_if_safe(4:5 + 0.5), 4:5 + 0.5)
  expect_identical(as_integer_if_safe(c(1, NA, 2)), c(1L, NA, 2L))
  expect_identical(as_integer_if_safe(c("abc", "def")), c("abc", "def"))
  max_int <- .Machine$integer.max
  expect_identical(as_integer_if_safe(c(-max_int, max_int)),
                   as.integer(c(-max_int, max_int)))
  expect_true(is.double(as_integer_if_safe(c(-max_int - 1, max_int))))
  expect_true(is.double(as_integer_if_safe(c(-max_int, max_int + 1))))
  expect_true(is.double(as_integer_if_safe(c(-max_int, NA, max_int + 1))))
  expect_equal(sum(is.na(as_integer_if_safe(c(-max_int, NA, max_int + 1)))), 1)

  expect_false(is.integer(as_integer_if_safe(-max_int + 0.5)))
  expect_false(is.integer(as_integer_if_safe(-max_int - 0.5)))
  expect_false(is.integer(as_integer_if_safe(c(-max_int, max_int, Inf, 0, 1, 2))))
  expect_equal(as_integer_if_safe(c(NA, NaN, 1, 2, 3)),
               as.integer(c(NA, NaN, 1, 2, 3)))

})

test_that("utils", {
  expect_equal(is_safe2int(1:10 + 0, 9), 0)
})

test_that("ubsan warning", {
  # NaN not naturally coercible to int
  expect_true(is.integer(as_integer_if_safe(NaN)))
  expect_true(is.integer(as_integer_if_safe(NA_real_)))
  expect_false(is.integer(as_integer_if_safe(Inf)))
  expect_false(is.integer(as_integer_if_safe(-Inf)))
  expect_error(force_as_integer(double(10), na_code = 0L), regexp = "Internal error")
})


