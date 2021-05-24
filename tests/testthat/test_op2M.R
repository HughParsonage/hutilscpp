test_that("op2M concordance", {
  operators <- c("!=", "==", ">=", "<=", ">", "<", "%in%", "%between%", "%(between)%",
                 "%]between[%")
  expect_equal(operators, M2op(seq_along(operators)))

  expect_equal(sapply(operators, op2M), sapply(operators, Cop2M))
})
