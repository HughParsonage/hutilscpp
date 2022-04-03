# test_that("between works", {

"%(between)%" <- hutilscpp:::`%(between)%`
"%]between[%" <- hutilscpp:::`%]between[%`
x <- 1:10
expect_equal(x %(between)% c(1, 10), 1:10 %in% 2:9)
expect_equal(x %]between[% c(1, 10), 1:10 %in% c(1, 10))
expect_equal(x %(between)% c(NA, 1L), between(x, NA_integer_, 1L, incbounds = FALSE))
expect_equal(x %(between)% c(1L, NA), between(x, 1L, NA_integer_, incbounds = FALSE))
expect_true(all(x %(between)% c(NA, NA)))
expect_equal(x %]between[% c(NA, 2L), x >= 2L)
expect_equal(x %]between[% c(2L, NA), x <= 2L)
expect_true(all(x %]between[% c(NA_integer_, NA)))

x <- seq.int(-.Machine$integer.max, .Machine$integer.max,
             by = 4096L)
ok <- as.raw(x >= -1e9L & x <= 1e9L)
expect_equal(hutilscpp:::Between(x, -1e9L, 1e9L, m = 0L), ok)
expect_equal(hutilscpp:::Between(x, -1e9L, 1e9L, m = 1L), ok)
expect_equal(hutilscpp:::Between(x, -1e9L, 1e9L, m = 2L), ok)
expect_equal(hutilscpp:::Between(x, -1e9L, 1e9L, m = 3L), ok)
expect_equal(hutilscpp:::Between(x, 5L, 4L), raw(length(x)))


