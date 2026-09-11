library(hutilscpp)

# Zero matches must never write into a zero-length index vector.
for (n in c(0L, 1L, 1001L)) {
  expect_identical(hutilscpp:::which_raw(raw(n)), integer(0))
}
mask <- as.raw(c(0, 1, 0, 255, 0, 0))
expect_identical(hutilscpp:::which_raw(mask), which(mask != as.raw(0)))
x <- rep(1L, 1001L)
expect_identical(and3s(x < 0L, type = "which"), integer(0))
expect_identical(or3s(x < 0L, type = "which"), integer(0))

# Both integer and promoted/forced double outputs must recycle scalar y.
for (nt in c(1L, 2L)) {
  expect_identical(abs_diff(1:4, 1L, nThread = nt), 0:3)
  expect_identical(abs_diff(1:4, 1L, nThread = nt, option = 2L), as.double(0:3))
  x <- c(-.Machine$integer.max, 0L, .Machine$integer.max)
  y <- .Machine$integer.max
  expect_identical(abs_diff(x, y, nThread = nt), abs(as.double(x) - y))
  expect_identical(abs_diff(1:4, 4:1, nThread = nt), abs(1:4 - 4:1))
}

# Scalar and vector comparisons must each append matches only once.
expect_identical(whichs(1L == 1L), 1L)
expect_identical(whichs(1L == 2L), integer(0))
x <- c(1L, 2L, 1L)
y <- c(1L, 1L, 1L)
expect_identical(whichs(x == y), which(x == y))
expect_identical(whichs(x == 1L), which(x == 1L))
expect_identical(whichs(integer() == 1L), integer(0))
expect_identical(whichs(1L == integer()), integer(0))
expect_identical(whichs(integer() == integer()), integer(0))
expect_identical(whichs(integer() < 1L), integer(0))

# Exercise every empty-input position in the NA and non-NA kernels.
for (and in c(TRUE, FALSE)) {
  for (other in list(TRUE, NA, c(TRUE, FALSE), c(TRUE, NA))) {
    for (empty in 1:3) {
      args <- list(other, other, other)
      args[[empty]] <- logical()
      expect_identical(do.call(which3, c(args, list(And = and))), integer(0))
    }
  }
  expect_identical(which3(logical(), logical(), logical(), And = and), integer(0))
  x <- c(TRUE, FALSE, TRUE)
  expect_identical(which3(x, TRUE, TRUE, And = and),
                   which(if (and) x & TRUE & TRUE else x | TRUE | TRUE))
}
expect_error(which3(c(TRUE, FALSE), rep(TRUE, 3), TRUE), "same length")
expect_error(which3(c(TRUE, NA), rep(TRUE, 3), TRUE), "same length")

# Validate before integer remainder, including values coerced to zero.
for (d in list(0L, 0, 0.5, FALSE)) {
  expect_error(divisible(1:3, d), "must not be zero")
}
expect_error(divisible(integer(), 0L), "must not be zero")
expect_identical(divisible(1:9, 3L), 1:9 %% 3L == 0L)
