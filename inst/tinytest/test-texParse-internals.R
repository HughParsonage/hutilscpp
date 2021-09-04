extractMandatory <- hutilscpp:::extractMandatory
where_square_bracket_opens <- hutilscpp:::where_square_bracket_opens


# test_that("where_square_bracket_opens", {
  s <- function(x) unlist(strsplit(x, split = "", fixed = TRUE))
  expect_equal(where_square_bracket_opens(s("0123[567]9"), -1), -1)
  expect_equal(where_square_bracket_opens(s("0123[567]9"), 0), -1)
  expect_equal(where_square_bracket_opens(s("0123[567]9"), 8L), 4L)
  expect_equal(where_square_bracket_opens(s("01234567]9"), 8L), 0L)
  expect_equal(where_square_bracket_opens(s("0123[{}4]9"), 8L), 4L)
  expect_equal(where_square_bracket_opens(s("{[[][]{}] "), 8L), 1L)
  expect_equal(where_square_bracket_opens(s(c("a",
                                              "[",
                                              "",
                                              "{",
                                              "a",
                                              "}",
                                              "]")),
                                            5L),
                                          1L)
  expect_equal(where_square_bracket_opens(s(c("a", # 0
                                              "[",
                                              "",  # 2
                                              "\u2010",
                                              "{", # 4
                                              "{",
                                              "a", # 6
                                              "}",
                                              "}", # 8
                                              "]")),
                                            8L),
               1L)
  # STRING_ELT(x, i) != 1
  expect_equal(where_square_bracket_opens(c("a", # 0
                                            "[",
                                            "",  # 2
                                            "aa",    # <---- len 2
                                            "{", # 4
                                            "{",
                                            "aa", # 6
                                            "}",
                                            "}", # 8
                                            "]"),
                                          9L),
               1L)


