test_that("pmin0 works", {
  o <- c(-1, 0, 1)
  expect_equal(pmin0(o), c(-1, 0, 0))
  pmin0(o, in_place = TRUE)
  expect_equal(o, c(-1, 0, 0))

  oi <- -1:5
  expect_equal(max(pmin0(oi)), 0L)
  expect_equal(max(oi), 5L)
  expect_identical(max(pmin0(oi, TRUE)), 0L)
  expect_equal(max(oi), 0L)
})
