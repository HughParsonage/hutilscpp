context("test-implies")

test_that("implies same as hutils", {
  skip_if_not_installed("hutils")
  skip_if_not_installed("data.table")
  library(data.table)
  library(hutils)
  DT <- CJ(x = c(TRUE,
                 NA,
                 FALSE),
           y = c(TRUE,
                 NA,
                 FALSE))
  DT[, hutils_orig := implies(x, y)]
  DT[, hutils_cpp := Implies(x, y)]
  expect_identical(DT$hutils_cpp, DT$hutils_cpp)
})
