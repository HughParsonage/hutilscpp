

test_that("where_square_bracket_opens", {
  s <- function(x) unlist(strsplit(x, split = "", fixed = TRUE))
  expect_equal(where_square_bracket_opens(s("0123[567]9"), -1), -1)
  expect_equal(where_square_bracket_opens(s("0123[567]9"), 0), -1)
  expect_equal(where_square_bracket_opens(s("0123[567]9"), 8L), 4L)
  expect_equal(where_square_bracket_opens(s("01234567]9"), 8L), 0L)
})
