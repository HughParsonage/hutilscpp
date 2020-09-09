test_that("rev works", {
  expect_equal(rev(1:5), 5:1)
  expect_equal(rev(1:5 + 0), 5:1)
})
