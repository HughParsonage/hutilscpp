context("test-tex")

test_that("extractMandatory works", {
  library(hutils)
  x <- c('a', '', 'b', 'd', '{', 'e', '}', '.')
  res <- extractMandatory(x, c('b', 'd'))
  expect_equal(res,
               if_else(nzchar(res), x, ''))
})
