context("test-are_even")

test_that("even works", {
  expect_equal(are_even(c(0, 1, 4)),
               c(TRUE, FALSE, TRUE))
  expect_equal(are_even(c(0L, 1L, 4L)),
               c(TRUE, FALSE, TRUE))
  expect_equal(are_even(-c(0L, 1L, 4L)),
               c(TRUE, FALSE, TRUE))
  expect_equal(which_are_even(c(0, 1, 4)),
               c(1L, 3L))
})

test_that("error handling", {
  expect_warning(are_even(c(0, 0.5, 1)),
                 regexp = "`x` was type double, but element 2 = 0.5 was not an integer value",
                 fixed = TRUE)
  expect_warning(which_are_even(c(0, 0.5, 1)),
                 regexp = "`x` was type double, but element 2 = 0.5 was not an integer value",
                 fixed = TRUE)
  expect_error(are_even("a"),
               "`x` was not an integer or double.",
               fixed = TRUE)
  expect_error(which_are_even("a"),
               "`x` was not an integer or double.",
               fixed = TRUE)
})
