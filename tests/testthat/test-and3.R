context("test-and3")

test_that("and3 error handling", {
  expect_error(and3(TRUE, logical(2), logical(3)))
})

test_that("and3 works", {
  x <- c(TRUE, FALSE, TRUE)
  y <- logical(3)
  z <- c(TRUE, TRUE, FALSE)
  expect_equal(and3(x, y, z),
               x & y & z)
  expect_equal(and3(x, y), x & y)
})

test_that("and3 length-1s", {
  expect_equal(and3(TRUE, TRUE, TRUE), TRUE)
  expect_equal(and3(TRUE, TRUE, FALSE), FALSE)
  expect_equal(and3(TRUE, TRUE, NA), NA)
})

test_that("and3 works with NAs", {
  skip_if_not_installed("data.table")
  library(data.table)
  DT <- CJ(xx = c(NA, FALSE, TRUE),
           yy = c(NA, FALSE, TRUE),
           zz = c(NA, FALSE, TRUE))
  DT[, res1 := and3(xx, yy, zz)]
  DT[, res2 := xx & yy & zz]
  expect_equal(DT[["res1"]], DT[["res2"]])

  DT[, res3 := and3(xx, yy, TRUE)]
  DT[, res4 := xx & yy & TRUE]
  expect_equal(DT[["res3"]], DT[["res4"]])

  for (pos in 1:3) {
    for (lgvl in c(TRUE, FALSE, NA)) {
      switch(pos,
             {
               DT[, res5 := and3(lgvl, xx, yy)]
             },
             {
               DT[, res5 := and3(xx, lgvl, yy)]
             },
             {
               DT[, res5 := and3(xx, yy, lgvl)]
             })
      DT[, res6 := xx & yy & lgvl]
      expect_equal(DT[["res5"]], DT[["res6"]],
                   info = paste("single", pos, lgvl))
    }
  }
})

