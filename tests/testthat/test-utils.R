context("test-utils")

test_that("checkTF works", {
  expect_null(check_TF(TRUE))
  expect_null(check_TF(FALSE))
  expect_error(check_TF(NA), "NA")
  expect_error(check_TF(1:2 > 0), "length")
  expect_error(check_TF(1L), "integer")
})
