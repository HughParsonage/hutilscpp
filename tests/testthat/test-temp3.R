test_that("temp3 works", {

  expect_true(test_intersect3_stdint())
  expect_true(test_intersect3_stdint(4))
  expect_true(test_intersect3_stdint(5))
})
