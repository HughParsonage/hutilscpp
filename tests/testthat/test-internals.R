

test_that("single_ox_x1_x2 works", {
  expect_true(test_single_ox_x1_x2(1L, op2M("!="), -1L, -1L))
  expect_true(test_single_ox_x1_x2(1L, op2M("=="), 1L, 1L))
  expect_true(test_single_ox_x1_x2(1L, op2M(">="), 0L, 0L))
  expect_true(test_single_ox_x1_x2(1L, op2M("%between%"), -1L, 2L))
  expect_true(test_single_ox_x1_x2(1L, op2M("%(between)%"), -1L, 2L))
  expect_false(test_single_ox_x1_x2(1L, op2M("%]between[%"), -1L, 2L))

  expect_true(test_single_ox_x1_x2(1, op2M("!="), -1, -1))
  expect_true(test_single_ox_x1_x2(1, op2M("=="), 1, 1))
  expect_true(test_single_ox_x1_x2(1, op2M(">="), 0, 0))
  expect_true(test_single_ox_x1_x2(1, op2M("%between%"), -1, 2))
  expect_true(test_single_ox_x1_x2(1, op2M("%(between)%"), -1, 2))
  expect_false(test_single_ox_x1_x2(1, op2M("%]between[%"), -1, 2))
})
