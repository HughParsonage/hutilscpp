expect_equal(bminmax(1), c(1, 1))
expect_equal(bminmax(1, nThread = 1L), c(1, 1))
expect_equal(bminmax(3), c(2, 4))
expect_equal(bminmax(c(3, 9)), c(2, 16))
expect_equal(bminmax(c(0.25, 8)), c(0.25, 8))
expect_equal(bminmax(c(0.3, 7.9)), c(0.25, 8))
expect_equal(bminmax(c(3L, 9L)), c(2, 16))
expect_equal(bminmax(.Machine$integer.max), c(2^30, 2^31))

smallest <- 2^-1074
expect_equal(bminmax(smallest), c(smallest, smallest))
expect_equal(bminmax(3 * smallest), c(2 * smallest, 4 * smallest))
expect_equal(bminmax(.Machine$double.xmax), c(2^1023, Inf))

# Exercise the dispatch thresholds, whole vectors, and scalar tails.
for (n in c(31L, 32L, 33L, 63L, 64L, 65L, 95L, 96L, 97L, 101L)) {
  xd <- seq(0.25, 31, length.out = n)
  xi_dispatch <- seq_len(n) + 2L
  expect_equal(bminmax(xd), hutilscpp:::.bminmax_portable(xd))
  expect_equal(bminmax(xi_dispatch), hutilscpp:::.bminmax_portable(xi_dispatch))
}
expect_equal(bminmax(c(rep(7, 100), 3)), c(2, 8))
expect_equal(bminmax(c(rep(7L, 100), 17L)), c(4, 32))

set.seed(47)
x <- exp(runif(10000, -700, 700))
reference <- 2^c(floor(log2(min(x))), ceiling(log2(max(x))))
expect_equal(bminmax(x), reference)
expect_equal(hutilscpp:::.bminmax_portable(x), reference)

xi <- sample.int(.Machine$integer.max, 10000, replace = TRUE)
reference_i <- 2^c(floor(log2(min(xi))), ceiling(log2(max(xi))))
expect_equal(bminmax(xi), reference_i)
expect_equal(hutilscpp:::.bminmax_portable(xi), reference_i)

expect_error(bminmax(numeric()), "non-empty")
expect_error(bminmax(logical()), "numeric")
expect_error(bminmax("1"), "numeric")
expect_error(bminmax(c(0, 1)), "positive")
expect_error(bminmax(c(-1, 1)), "positive")
expect_error(bminmax(c(NA_real_, 1)), "positive")
expect_error(bminmax(c(NaN, 1)), "positive")
expect_error(bminmax(c(Inf, 1)), "positive")
expect_error(bminmax(c(-Inf, 1)), "positive")
expect_error(bminmax(c(NA_integer_, 1L)), "positive")
expect_error(bminmax(1, nThread = NA_integer_), "nThread")
expect_error(bminmax(1, nThread = 1.5), "whole number")
expect_error(bminmax(1, nThread = 0L), "positive whole number")
expect_error(bminmax(1, nThread = -1L), "positive whole number")

for (bad_at in c(1L, 16L, 17L, 32L, 33L, 64L, 65L, 96L, 101L)) {
  bad_double <- rep(1, 101)
  bad_integer <- rep(1L, 101)
  bad_double[bad_at] <- NA_real_
  bad_integer[bad_at] <- NA_integer_
  expect_error(bminmax(bad_double), "positive")
  expect_error(bminmax(bad_integer), "positive")
}

expect_true(is.logical(hutilscpp:::.bminmax_has_avx512()))
expect_equal(length(hutilscpp:::.bminmax_has_avx512()), 1L)

if (hutilscpp:::has_openmp() && parallel::detectCores() >= 2L) {
  n_thread <- min(4L, parallel::detectCores())
  set.seed(48)
  x_parallel <- runif(1000000, 0.25, 2^20)
  xi_parallel <- sample.int(.Machine$integer.max, 2500000, replace = TRUE)
  expect_equal(bminmax(x_parallel, nThread = n_thread), bminmax(x_parallel))
  expect_equal(bminmax(xi_parallel, nThread = n_thread), bminmax(xi_parallel))

  x_parallel[900001L] <- NA_real_
  xi_parallel[2200001L] <- NA_integer_
  expect_error(bminmax(x_parallel, nThread = n_thread), "positive")
  expect_error(bminmax(xi_parallel, nThread = n_thread), "positive")
}
