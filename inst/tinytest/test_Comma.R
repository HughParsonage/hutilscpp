library(hutilscpp)

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

