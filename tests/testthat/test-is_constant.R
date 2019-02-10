context("test-is_constant")

test_that("is_constant works", {
  expect_true(is_constant(NULL))
  expect_true(is_constant(integer(0)))
  expect_true(is_constant(integer(1)))

  expect_true(is_constant(integer(10)))
  expect_true(is_constant(double(10)))
  expect_true(is_constant(character(10)))
  expect_true(is_constant(as.factor(character(10))))
  expect_true(is_constant(raw(10)))

  expect_false(is_constant(c(integer(10), 1L)))
  expect_false(is_constant(c(integer(10), 1)))
  expect_false(is_constant(c(character(10), 1)))
  expect_false(is_constant(c(character(10), 1)))
  expect_false(is_constant(c(raw(10), as.raw(1))))
  expect_false(is_constant(c(factor(c(character(10), 1)))))

})
