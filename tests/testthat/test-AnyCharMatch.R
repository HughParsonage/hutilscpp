context("AnyCharMatch")

test_that("AnyCharMatch works and opposite", {
  x <- c("", "", "a")
  o1 <- AnyCharMatch(x, "")
  expect_equal(o1, 1L)
  o3 <- AnyCharMatch(x, "", opposite = TRUE)
  expect_equal(o3, 3L)
  o3 <- AnyCharMatch(x, "a", FALSE)
  expect_equal(o3, 3L)
})
