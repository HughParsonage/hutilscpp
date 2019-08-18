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
})

test_that("utils", {
  expect_equal(is_safe2int(1:10 + 0, 9), 0)
})
