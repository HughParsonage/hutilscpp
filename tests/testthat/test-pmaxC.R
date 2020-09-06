test_that("pmaxC works", {
  x <- 1:10
  expect_equal(pmaxC(x, 3L), pmax(x, 3L))
  expect_equal(pmaxC(x, 3), pmax(x, 3))
})

test_that("pmaxC error handling", {
  expect_error(pmaxC("", ""), regexp = "numeric")
  expect_error(pmaxC(list(1), 1L), regexp = "atomic")
  expect_error(pmaxC(1:5, 1:2), "length.*2")
  expect_message(pmaxC(1:5, 0.5), "Output is double.")
})
