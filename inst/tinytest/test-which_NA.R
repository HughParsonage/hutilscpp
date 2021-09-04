# test_that("which_NA works", {
which_NA <- hutilscpp:::which_NA
which_notNA <- hutilscpp:::which_notNA
  expect_true(TRUE)
  expect_equal(which_NA(c(letters)), integer(0))
  expect_equal(which_NA(c(NA, NA)), 1:2)
  expect_equal(which_NA(c(NA, NA, FALSE)), 1:2)
  expect_equal(which_NA(c(NA, NA, 1L)), 1:2)
  expect_equal(which_NA(c(NA, NA, 1)), 1:2)
  expect_equal(which_NA(c(NA, NA, '')), 1:2)
  expect_equal(which_NA(c(NA_real_ + 1i, 0, 1i)), 1L)


# test_that("which_notNA works", {
  expect_equal(which_notNA(double(5)), 1:5)
  expect_equal(which_notNA(c(NA, NA)), integer(0))
  expect_equal(which_notNA(c(NA, NA, FALSE)), 3L)
  expect_equal(which_notNA(c(NA, NA, 1L)), 3L)
  expect_equal(which_notNA(c(NA, NA, 1)), 3L)
  expect_equal(which_notNA(c(NA, NA, '')), 3L)
  expect_equal(which_notNA(c(NA_real_ + 1i, 0, 1i)), 2:3)

