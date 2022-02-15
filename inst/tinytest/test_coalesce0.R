library(hutilscpp)
.coalesce0 <- function(x) {
  x[is.na(x)] <- FALSE
  x
}
expect_equal(coalesce0(logical(3)), logical(3))
expect_equal(coalesce0(c(NA, logical(3))), logical(4))

x <- c(1L, -.Machine$integer.max, .Machine$integer.max, 0L)
expect_equal(coalesce0(x), .coalesce0(x))
y <- rep_len(x, 1001)
expect_equal(coalesce0(y), .coalesce0(y))
x <- c(NA, x)
expect_equal(coalesce0(x), .coalesce0(x))
x <- c(NA, x, NA)
x <- rep_len(x, 1001)
expect_equal(coalesce0(x), .coalesce0(x))
y <- rep_len(x, 1001)
expect_equal(coalesce0(y), .coalesce0(y))

x <- as.double(x)
expect_equal(coalesce0(x), .coalesce0(x))
y <- rep_len(x, 1001)
expect_equal(coalesce0(y), .coalesce0(y))
x <- c(NA, x)
expect_equal(coalesce0(x), .coalesce0(x))
x <- c(NA, x, NA)
x <- rep_len(x, 1001)
expect_equal(coalesce0(x), .coalesce0(x))
y <- rep_len(x, 1001)
expect_equal(coalesce0(y), .coalesce0(y))

