context("test-which_true_onwards")

test_that("multiplication works", {
  expect_identical(which_true_onwards(c(TRUE, TRUE, FALSE, TRUE, TRUE)),
                   4L)
})
