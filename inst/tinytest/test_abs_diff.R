library(hutilscpp)

x <- c(5L, 3L, 2L, 8L)
y <- x + 0L
expect_equal(abs_diff(x, y), integer(length(x)))
expect_equal(abs_diff(x, y, option = 1), integer(length(x)))
expect_equal(max_abs_diff(x, y), max(abs(x - y)))
expect_equal(max_abs_diff(x, 6), max(abs(x - 6)))
expect_equal(max_abs_diff(6, x), max(abs(x - 6)))
expect_equal(max_abs_diff(x, 6.1), max(abs(x - 6.1)))
expect_equal(max_abs_diff(6.1, x), max(abs(x - 6.1)))

x <- c(5L, 3L, 2L, 8L)
y <- c(4L, -1L, 2L, 3L)
expect_equal(abs_diff(x, y), as.integer(abs(x - y)))
expect_equal(abs_diff(x, y, option = 3L), which.max(abs(x - y)))

x <- c(0L, -.Machine$integer.max, .Machine$integer.max)
y <- c(-1L, -2L, 1L)
expect_equal(abs_diff(x, y), as.integer(abs(x - y)))
expect_true(is.integer(abs_diff(x, y)))
expect_true(is.integer(abs_diff(x, y, option = 0L)))
expect_false(is.integer(abs_diff(x, y, option = 2L)))

x <- as.double(x)
y <- as.double(y)
expect_equal(abs_diff(x, y), abs(x - y))
expect_equal(max_abs_diff(x, y), max(abs(x - y)))
expect_equal(abs_diff(x, y, option = 3L), which.max(abs(x - y)))
y <- 1.5
expect_equal(abs_diff(x, y), abs(x - y))
expect_equal(max_abs_diff(x, y), max(abs(x - y)))
expect_equal(abs_diff(x, y, option = 3L), which.max(abs(x - y)))
