library(hutilscpp)

x <- c(5L, 3L, 2L, 8L)
y <- x + 0L
expect_equal(abs_diff(x, y), integer(length(x)))

x <- c(5L, 3L, 2L, 8L)
y <- c(4L, -1L, 2L, 3L)
expect_equal(abs_diff(x, y), as.integer(abs(x - y)))

x <- c(0L, -.Machine$integer.max, .Machine$integer.max)
y <- c(-1L, -2L, 1L)
expect_equal(abs_diff(x, y), as.integer(abs(x - y)))
expect_true(is.integer(abs_diff(x, y)))
expect_true(is.integer(abs_diff(x, y, option = 0L)))
expect_false(is.integer(abs_diff(x, y, option = 2L)))

