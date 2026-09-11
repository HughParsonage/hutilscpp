library(hutilscpp)
expect_equal(hutilscpp:::test_width_dbl(0, 1L), nchar("0.0"))

x <- c(1626783884L, 969909421L, 205541854L, -1L, 0L, 1L, -1214788235L,
       -709260613L, -795055625L)
expect_equal(Comma(x), prettyNum(x, big.mark = ","))
expect_equal(Comma(1234L, digits = -1L, big.mark = "_"), "1_230")
expect_equal(Comma(1234.1, digits = -1L, big.mark = "_"), "1_230")
expect_equal(Comma(1234.1, digits = 1L, big.mark = "_"), "1_234.1")
expect_equal(Comma(0, digits = 1L, big.mark = "_"), "0.0")
expect_equal(Comma(0.123453, digits = 5L, big.mark = "_"), "0.12345")
expect_equal(Comma(9843.123453, digits = 5L, big.mark = "'"), "9'843.12345")

# test 1234.1 != '1234.0
expect_equal(Comma(1234.1, digits = 1L, big.mark = "/"), "1/234.1")

# A leading minus sign must not shift the thousands separators.
x <- -c(1, 12, 123, 1234, 12345, 123456, 1234567, 12345678, 123456789)
for (big_mark in c(",", " ", "'", "_", "~", '"', "/")) {
  expect_identical(Comma(x, big.mark = big_mark),
                   prettyNum(x, big.mark = big_mark, scientific = FALSE))
  expect_identical(Comma(x - 0.25, digits = 2L, big.mark = big_mark),
                   paste0(prettyNum(x, big.mark = big_mark, scientific = FALSE), ".25"))
}
expect_identical(Comma(c(NA_integer_, -.Machine$integer.max, 0L)),
                 c("NA", "-2,147,483,647", "0"))
expect_identical(Comma(NA_integer_), Comma(NA_real_))



