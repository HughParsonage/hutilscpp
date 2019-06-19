test_that("which3 works", {
  expect_equal(which3(c(TRUE, FALSE, FALSE),
                      c(FALSE, TRUE, FALSE),
                      c(FALSE, FALSE, TRUE)),
               integer(0))
  expect_equal(which3(c(TRUE, FALSE, FALSE),
                      c(FALSE, TRUE, FALSE),
                      c(FALSE, FALSE, TRUE),
                      And = FALSE),
               1:3)
})


test_that("which3 prepare", {
  x <- rep(TRUE, 10)
  y <- 1:10 %in% c(1, 2, 3, 10)
  z <- 1:10 %in% c(2, 3, 10)
  expect_equal(which3(x, y, z), which(x & y & z))
  expect_equal(which3(x, y, z, prepare = TRUE), which(x & y & z))
  expect_equal(do_which3_prepare1(which(z), y, x), which(x & y & z))


})

