test_that("sum_or3s works", {
  sum_bor3 <- function(exprA, exprB = TRUE, exprC = TRUE, ..., .parent_nframes = 1L, nThread = 1L) {
    if (missing(..1)) {
      sum(exprA | exprB | exprC)
    } else if (missing(..2)) {
      return(exprA | exprB)
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
  expect_equal(DT[, sum_or3s(A %in% c(5L, 5L, 6L), B != 2L, Z %between% c(1L, 4L))],
               DT[, sum_bor3(A %in% c(5L, 5L, 6L), B != 2L, Z %between% c(1L, 4L))])
  expect_equal(DT[, sum_or3s(A %in% c(5L, 5L, 6L), Z %between% c(1L, 4L))],
               DT[, sum_bor3(A %in% c(5L, 5L, 6L), Z %between% c(1L, 4L))])
  expect_equal(DT[, sum_or3s(A %in% c(5L, 5L, 6L), B != 2L, Z %in% 1:4)],
               DT[, sum_bor3(A %in% c(5L, 5L, 6L), B != 2L, Z %in% 1:4)])
  expect_equal(DT[, sum_or3s(A %in% c(5L, 5L, 6L), B != 2L, Z %in% c(2L, 1:4))],
               DT[, sum_bor3(A %in% c(5L, 5L, 6L), B != 2L, Z %in% c(2L, 1:4))])
  expect_equal(DT[, sum_or3s(A %in% c(5L, integer(101)), B != 2L, Z %in% c(2L, 1:4))],
               DT[, sum_bor3(A %in% c(5L, integer(101)), B != 2L, Z %in% c(2L, 1:4))])
  expect_equal(DT[, sum_or3s(A %in% c(5L, integer(101)), B %in% c(100:1001), Z %in% c(integer(101)))],
               DT[, sum_bor3(A %in% c(5L, integer(101)), B %in% c(100:1001), Z %in% c(integer(101)))])
})
