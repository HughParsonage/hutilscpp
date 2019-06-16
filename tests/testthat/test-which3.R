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
