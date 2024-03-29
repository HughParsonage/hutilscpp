library(hutilscpp)
library(tinytest)
do_and3 <- hutilscpp:::do_and3
na_and <- hutilscpp:::na_and



# test_that("and3 error handling", {
  expect_error(and3(logical(2), logical(3)), pattern = "permissible.*lengths")
  expect_error(and3(z = TRUE, logical(2), logical(3)), pattern = "permissible.*lengths")
  expect_error(and3(y = TRUE, logical(2), logical(3)), pattern = "permissible.*lengths")
  expect_error(and3(y = TRUE, logical(2), x = logical(3)), pattern = "permissible.*lengths")
  expect_error(and3(x = TRUE, logical(2), logical(3)), pattern = "permissible.*lengths")
  expect_error(and3(x = logical(3), logical(2), logical(3)), pattern = "permissible.*lengths")
  expect_error(and3(x = logical(4), logical(1), logical(3)), pattern = "permissible.*lengths")
  expect_error(and3(y = logical(4), logical(1), logical(3)), pattern = "permissible.*lengths")
# })

# test_that("and3 works", {
  x <- c(TRUE, FALSE, TRUE)
  y <- logical(3)
  z <- c(TRUE, TRUE, FALSE)
  expect_equal(and3(x, y, z),
               x & y & z)
  expect_equal(and3(x, y), x & y)
# })

# test_that("and3 length-1s", {
  expect_equal(and3(TRUE, TRUE, TRUE), TRUE)
  expect_equal(and3(TRUE, TRUE, FALSE), FALSE)
  expect_equal(and3(TRUE, TRUE, NA), NA)
# })

# test_that("length-1s 2/3", {
  expect_equal(and3(TRUE, TRUE, c(TRUE, FALSE, TRUE)),
               c(TRUE, FALSE, TRUE))
  expect_equal(and3(TRUE, logical(3), c(TRUE, FALSE, TRUE)),
               c(FALSE, FALSE, FALSE))
# })

# test_that("and3 works with NAs", {

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
  for (x in c(TRUE, FALSE, NA)) {
    for (y in c(TRUE, FALSE, NA)) {
      for (z in c(TRUE, FALSE, NA)) {
        expect_equal(and3(c(x, z), y, x),
                     c(x, z) & y & x,
                     info = paste("1", x, y, z))
        expect_equal(and3(y = c(x, y), y, x),
                     c(x, y) & y & x,
                     info = paste("2", x, y, z))
      }
    }
  }


# test_that("nas_absent ok", {
  expect_equal(and3(c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE), nas_absent = TRUE),
               c(TRUE, FALSE))
# })

# test_that("C++", {
  expect_error(do_and3(logical(3), logical(2), TRUE),
               pattern = "lengths")
  expect_error(do_and3(c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE, FALSE)),
               pattern = "wrong length")
  expect_equal(do_and3(c(TRUE, FALSE), c(TRUE, FALSE), TRUE), c(TRUE, FALSE))
  expect_equal(do_and3(c(TRUE, FALSE), c(TRUE, FALSE), FALSE), c(FALSE, FALSE))
  expect_equal(do_and3(c(TRUE, FALSE), c(TRUE, FALSE), logical(0)), c(TRUE, FALSE))
  expect_equal(na_and(c(TRUE, FALSE, NA)), c(TRUE, FALSE, NA) & NA)
  expect_error(do_and3(c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE, FALSE)),
               pattern = "wrong length")
# })









