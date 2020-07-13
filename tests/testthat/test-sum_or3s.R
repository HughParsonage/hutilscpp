test_that("sum_or3s works", {
  sum_bor3 <- function(exprA, exprB, exprC, ..., .parent_nframes = 1L, nThread = 1L) {
    if (missing(..1)) {
      sum(exprA | exprB | exprC)
    } else {
      sum_bor3(exprA | exprB, exprC, ...)
    }
  }
  abc <- 1:100
  def <- 1:100
  ghi <- 1:100
  x <- abc == 1
  expect_equal(sum_or3s(x, def >= 1, ghi <= 2), 100)
  expect_equal(sum_or3s(x, x, x), 1)

  expect_equal(sum_or3s(abc == 1, def >= 1, ghi <= 2), 100)

  DT <- data.table(A = 1:100, B = 2L, Z = 3L)
  expect_equal(DT[, sum_or3s(A %in% c(5L, 5L, 6L), B == 2L, Z >= 1L)],
               DT[, sum_bor3(A %in% c(5L, 5L, 6L), B == 2L, Z >= 1L)])
  expect_equal(DT[, sum_or3s(A %in% c(5L, 5L, 6L), B != 2L, Z >= 1L)],
               DT[, sum_bor3(A %in% c(5L, 5L, 6L), B != 2L, Z >= 1L)])
})
