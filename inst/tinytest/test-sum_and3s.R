library(hutilscpp)
library(data.table)
"%(between)%" <- hutilscpp:::`%(between)%`
"%]between[%" <- hutilscpp:::`%]between[%`

library(tinytest)
sum_band3 <- function(exprA, exprB = TRUE, exprC = TRUE, ..., .parent_nframes = 1L, nThread = 1L) {
  if (missing(..1)) {
    sum(exprA & exprB & exprC)
  } else {
    sum_band3(exprA & exprB, exprC, ...)
  }
}
expect_equal(sum_and3s(1:100 > 101, 1:100 > 101,,1:100 > 101), 0)
expect_equal(sum_and3s(1:100 > 101,, 1:100 > 101,,,1:100 > 101), 0)
expect_equal(sum_and3s(1:100 > 101,,, 1:100 > 101,,,1:100 > 101), 0)
expect_equal(sum_and3s(1:100 > 101, 1:100 > 101,,,1:100 > 101), 0)
expect_equal(sum_or3s(1:100 > 101, 1:100 > 101,,1:100 > 101), 0)
expect_equal(sum_or3s(1:100 > 101,, 1:100 > 101,,1:100 > 101), 0)
expect_equal(sum_or3s(1:100 > 101,,, 1:100 > 101,,1:100 > 101), 0)
expect_equal(sum_or3s(1:100 > 101, 1:100 > 101,,,1:100 > 101), 0)

expect_equal(sum_and3s(1:1001 > NaN), 0)
expect_equal(sum_and3s(1:1001 < NaN), 0)
expect_equal(sum_and3s(seq(0.5, 100, length.out = 1001) %between% c(5, 9.2)),
             sum_band3(seq(0.5, 100, length.out = 1001) %between% c(5, 9.2)))
expect_equal(sum_and3s(rep_len(c(TRUE, FALSE), 2e3 + 1) %between% c(TRUE, TRUE)), 1e3 + 1)
expect_equal(sum_and3s(rep_len(c(TRUE, FALSE), 2e3) %between% c(FALSE, FALSE)), 1e3)
expect_equal(sum_and3s(1:1001 < -3e9), 0)


# test_that("sum_and3s works", {
abc <- -1:100
def <- -1:100
ghi <- -1:100
x <- abc == 1
expect_equal(sum_and3s(x, def >= 1, ghi <= 2), 1)
expect_equal(sum_and3s(!x, def >= 1, ghi <= 2), 1)
expect_equal(sum_and3s(!x, def >= 1, ghi >= 2), 99)
expect_equal(sum_and3s(x, x, x), 1)
expect_equal(sum_and3s(!x, x, x), 0)
expect_equal(sum_and3s(!x, , x), 0)
ox <- 50L
expect_equal(sum_and3s(!x, !x, abc == ox), 1)
expect_equal(sum_and3s(!x, !x, !x), length(x) - 1)

expect_equal(sum_and3s(abc == 1, def >= 1, ghi <= 2), 1)

DT <- data.table(A = 1:100, B = 2L, Z = 3L)
expect_equal(DT[, sum_and3s(A %in% c(5L, 5L, 6L), B == 2L, Z >= 1L)],
             DT[, sum_band3(A %in% c(5L, 5L, 6L), B == 2L, Z >= 1L)])
expect_equal(DT[, sum_and3s(A %between% c(5L, 6L), B == 2L, Z >= 1L)],
             DT[, sum_band3(A %between% c(5L, 6L), B == 2L, Z >= 1L)])
expect_equal(DT[, sum_and3s(A %]between[% c(5L, 60L), B == 2L, Z >= 1L)],
             DT[, sum_band3(A %]between[% c(5L, 60L), B == 2L, Z >= 1L)])
expect_equal(DT[, sum_and3s(A %(between)% c(5L, 60L), B == 2L, Z >= 1L)],
             DT[, sum_band3(A %(between)% c(5L, 60L), B == 2L, Z >= 1L)])
expect_equal(DT[, sum_and3s(A %in% c(5L, 6L), B == 2L, Z >= 1L)],
             DT[, sum_band3(A %in% c(5L, 6L), B == 2L, Z >= 1L)])
expect_equal(DT[, sum_and3s(A %in% B, B == 2L, Z >= 1L)],
             DT[, sum_band3(A %in% B, B == 2L, Z >= 1L)])
expect_equal(DT[, sum_and3s(B == 2L, A %in% B, B == 2L, Z >= 1L)],
             DT[, sum_band3(B == 2L, A %in% B, B == 2L, Z >= 1L)])
expect_equal(DT[, sum_and3s(B == 2L, A %in% c(5:6, 5L), B == 2L, Z >= 1L)],
             DT[, sum_band3(B == 2L, A %in% c(5:6, 5L), B == 2L, Z >= 1L)])
expect_equal(DT[, sum_and3s(B == 2L, A %in% c(5:6), B == 2L, Z >= 1L)],
             DT[, sum_band3(B == 2L, A %in% c(5:6), B == 2L, Z >= 1L)])
expect_equal(DT[, sum_and3s(B == 2L, B == 2L, A %in% c(6:5), B == 2L, Z >= 1L)],
             DT[, sum_band3(B == 2L, B == 2L, A %in% c(6:5), B == 2L, Z >= 1L)])
expect_equal(DT[, sum_and3s(B == 2L, B == 2L, A %in% B, B == 2L, Z >= 1L)],
             DT[, sum_band3(B == 2L, B == 2L, A %in% B, B == 2L, Z >= 1L)])


# })

# test_that("sum_and/or3s doubles", {
x <- rep(50.5, 10)

# Would be zero if coerced to integer wrongly
expect_equal(sum_and3s(x > 50, x < 51), 10)
expect_equal(sum_and3s(x > 50.1, x < 50.9), 10)
expect_equal(sum_and3s(x %(between)% c(50.1, 50.9)), 10)
expect_equal(sum_and3s(x %]between[% c(50.1, 50.9)),
             sum_band3(x %]between[% c(50.1, 50.9)))
expect_equal(sum_and3s(x %]between[% c(50.1, 55.9)),
             sum_band3(x %]between[% c(50.1, 55.9)))
expect_equal(sum_and3s(x != 5.5),
             sum_band3(x != 5.5))
expect_equal(sum_and3s(x != 50.5),
             sum_band3(x != 50.5))
expect_equal(sum_or3s(x > 50, x < 51), 10)
expect_equal(sum_or3s(x >= 50, x <= 51), 10)
expect_equal(sum_or3s(x > 50.1, x < 50.9), 10)
# })

# test_that("sum_and3s decompose_expr", {
x <- c(10L, 20L, 15L, 55:100, integer(100))
expect_equal(sum_and3s(x >= 0L, x %in% x, x > -0.5),
             length(x))
expect_equal(sum_and3s(x %in% x, x %in% x, x %in% x),
             length(x))
expect_equal(sum_and3s(x > -0.5, x > -0.5, x > -0.5),
             length(x))
# })

x <- c(x, NA, x)
expect_equal(sum(x == 55L, na.rm = TRUE),
             sum_and3s(x == 55L))
expect_equal(sum(x >= 55L & x == "55", na.rm = TRUE),
             sum_and3s(x >= 55L, x == "55"))

x <- c(TRUE, NA, FALSE)
expect_equal(sum(x, na.rm = TRUE), sum_and3s(x))
