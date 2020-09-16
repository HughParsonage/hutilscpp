test_that("between works", {
  x <- 1:10
  expect_equal(x %(between)% c(1, 10), 1:10 %in% 2:9)
  expect_equal(x %]between[% c(1, 10), 1:10 %in% c(1, 10))
})
