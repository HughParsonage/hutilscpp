context("test-cumsum_reset")

test_that("Error handling", {
  expect_error(cumsum_reset(1),
               regexp = "logical")
})

test_that("cumsum_reset works", {
  expect_identical(cumsum_reset(c(TRUE, TRUE, FALSE, TRUE, TRUE)),
                   c(1L, 2L, 0L, 1L, 2L))
  expect_identical(cumsum_reset(x = c(TRUE, TRUE, FALSE, TRUE, TRUE),
                                y = 1:5),
                   c(1L, 3L, 0L, 4L, 9L))

})
