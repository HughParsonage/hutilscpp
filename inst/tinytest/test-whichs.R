# test_that("whichs works", {
  xox <- rep_len(1:10, 2)
  yoy <- rep_len(1:10, length(xox))
  expect_equal(whichs(xox != 1), which(xox != 1))
  expect_equal(whichs(xox != 1L), which(xox != 1L))
  expect_equal(whichs(xox == 1), which(xox == 1))
  expect_equal(whichs(xox == 1.5), which(xox == 1.5))
  expect_equal(whichs(xox >= 1), which(xox >= 1))
  expect_equal(whichs(xox <= 1), which(xox <= 1))
  expect_equal(whichs(xox < 1), which(xox < 1))
  expect_equal(whichs(xox > 1), which(xox > 1))
  expect_equal(whichs(xox %in% 1), which(xox %in% 1))
  expect_equal(whichs(identity(xox %in% 1)), which(identity(xox %in% 1)))


  expect_equal(whichs(xox != yoy), which(xox != yoy))
  expect_equal(whichs(xox == yoy), which(xox == yoy))
  expect_equal(whichs(xox >= yoy), which(xox >= yoy))
  expect_equal(whichs(xox <= yoy), which(xox <= yoy))
  expect_equal(whichs(xox < yoy), which(xox < yoy))
  expect_equal(whichs(xox > yoy), which(xox > yoy))

