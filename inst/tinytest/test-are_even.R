#context "test-are_even")

# test_that("even works", {
expect_equal(are_even(c(0, 1, 4)),
             c(TRUE, FALSE, TRUE))
expect_equal(are_even(c(0L, 1L, 4L)),
             c(TRUE, FALSE, TRUE))
expect_equal(are_even(-c(0L, 1L, 4L)),
             c(TRUE, FALSE, TRUE))
expect_equal(which_are_even(c(0, 1, 4)),
             c(1L, 3L))
expect_identical(which_are_even(c(0L, 1L, 4L)),
                 c(1L, 3L))
expect_identical(which_are_even(c(5L, 1L, 3L)),
                 integer(0L))
expect_identical(which_are_even(c(0, rep(1, 1e5))),
                 1L)


# test_that("error handling", {
expect_error(are_even("foo"))
expect_warning(are_even(c(0, 0.5, 1)),
               pattern = "`x` was type double, but element 2 = 0.5 was not an integer value")

expect_warning(which_are_even(c(0, 0.5, 1)),
               pattern = "`x` was type double, but element 2 = 0.5 was not an integer value")

expect_error(are_even("a"),
             "`x` was not an integer or double.")

expect_error(which_are_even("a"),
             "`x` was not an integer or double.")



# test_that("long vectors", {
if (at_home()) {
  expect_error(which_are_even(integer(1e10)),
               # long for 64 bit, large for 32bit
               pattern = "long|large")
}

# test_that("non-finite values", {
expect_warning(are_even(c(NA, 1)))
expect_warning(are_even(c(NaN, 1)))
expect_warning(are_even(c(Inf, 1)))
expect_warning(are_even(c(-Inf, 1)))

expect_warning(which_are_even(c(NA, 1)))
expect_warning(which_are_even(c(NaN, 1)))
expect_warning(which_are_even(c(Inf, 1)))
expect_warning(which_are_even(c(-Inf, 1)))

expect_warning(wh <- which_are_even(c(NA, 1, 2, Inf, -Inf, 7, 8, 8)))
expect_equal(wh, which((c(NA, 1, 2, Inf, -Inf, 7, 8, 8) %% 2) == 0))



# test_that("large", {
if (requireNamespace("withr", quietly = TRUE)) {
  withr::with_options(list(warn = -1), {
    expect_true(all(are_even(c(0L, 10L, 1e10))))
  })
}


# test_that("keep_nas", {
expect_true(all(are_even(c(0L, 10L), nThread = 1L, keep_nas = FALSE)))
if (at_home()) {
  expect_false(any(are_even(c(11L, 13L), nThread = 2L, keep_nas = FALSE)))
}
