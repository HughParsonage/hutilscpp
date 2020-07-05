test_that("sum_and3s works", {
  sum_band3 <- function(exprA, exprB, exprC, ..., .parent_nframes = 1L, nThread = 1L) {
    if (missing(..1)) {
      sum(exprA & exprB & exprC)
    } else {
      sum_band3(exprA & exprB, exprC, ...)
    }
  }
  abc <- -1:100
  def <- -1:100
  ghi <- -1:100
  x <- abc == 1
  expect_equal(sum_and3s(x, def >= 1, ghi <= 2), 1)
  expect_equal(sum_and3s(!x, def >= 1, ghi <= 2), 1)
  expect_equal(sum_and3s(!x, def >= 1, ghi >= 2), 99)
  expect_equal(sum_and3s(x, x, x), 1)
  expect_equal(sum_and3s(!x, x, x), 0)
  ox <- 50L
  expect_equal(sum_and3s(!x, !x, abc == ox), 1)
  expect_equal(sum_and3s(!x, !x, !x), length(x) - 1)

  expect_equal(sum_and3s(abc == 1, def >= 1, ghi <= 2), 1)

  DT <- data.table(A = 1:100, B = 2L, Z = 3L)
  expect_equal(DT[, sum_and3s(A %in% c(5L, 5L, 6L), B == 2L, Z >= 1L)],
               DT[, sum_band3(A %in% c(5L, 5L, 6L), B == 2L, Z >= 1L)])
  expect_equal(DT[, sum_and3s(A %between% c(5L, 6L), B == 2L, Z >= 1L)],
               DT[, sum_band3(A %between% c(5L, 6L), B == 2L, Z >= 1L)])
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


})
