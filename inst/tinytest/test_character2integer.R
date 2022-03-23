library(hutilscpp)

x <- c(1626783884L, 969909421L, 205541854L, -1L, 0L, 1L, -1214788235L,
       -709260613L, -795055625L)
cx <- prettyNum(x, big.mark = ",")
expect_equal(character2integer(cx), x)
cx <- prettyNum(x)
expect_equal(character2integer(cx), x)
x <- as.double(x)
cx <- prettyNum(x, big.mark = ",")
expect_equal(character2integer(cx), x)
cx <- prettyNum(x)
expect_equal(character2integer(cx), x)

x <- c(1L, 3L, 7L, 17L, 43L, 105L, 254L, 616L, 1493L, 3616L, 8761L,
       21225L, 51417L, 124557L, 301736L, 730948L, 1770704L, 4289486L,
       10391170L, 25172341L, 60979343L, 147720878L, 357849998L, 866882343L,
       2100000000L)
x <- c(x, -5:5, -x)
cx <- as.character(x)
expect_equal(character2integer(cx), x)
cx <- Comma(x, big.mark = "_")
expect_equal(character2integer(cx), x)

expect_equal(character2integer("12345678901", allow.double = NA), NA_integer_)
expect_equal(character2integer("12345678901", allow.double = TRUE), 12345678901)
expect_error(character2integer("12345678901", allow.double = FALSE))

expect_equal(Comma(5700000.05, digits = 2), "5,700,000.05")

expect_equal(character2integer(" -5,000.5", allow.double = TRUE), -5000.5)

expect_equal(character2integer(" -7,000", na.strings = NA_character_), -7000)
expect_equal(character2integer(c(" -7,000", NA), na.strings = NA_character_), c(-7000L, NA))
