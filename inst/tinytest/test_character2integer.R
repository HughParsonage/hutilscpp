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
cx <- Comma(x, big.mark = "'")
expect_equal(character2integer(cx), x)

expect_equal(character2integer("12345678901", allow.double = NA), NA_integer_)
expect_equal(character2integer("12345678901", allow.double = TRUE), 12345678901)
expect_error(character2integer("12345678901", allow.double = FALSE))

expect_equal(Comma(5700000.05, digits = 2), "5,700,000.05")

expect_equal(character2integer(" -5,000.5", allow.double = TRUE), -5000.5)

expect_equal(character2integer(" -7,000", na.strings = NA_character_), -7000)
expect_equal(character2integer(c(" -7,000", NA), na.strings = NA_character_), c(-7000L, NA))
expect_true(is.integer(character2integer("1234.00")))
expect_true(is.integer(character2integer("2,012,345,345.00000")))
expect_true(identical(character2integer(c(NA, "1  234  567  890"), na.strings = "NA"),
                      c(NA, 1234567890L)))


expect_equal(Comma(c(NA, 50, 1234.44, -14.1, Inf, -Inf), digits = 2L),
             c("NA", "50.00", "1,234.44", "-14.10", "Inf", "-Inf"))
expect_equal(character2integer(c(NA, "5,300")), c(NA, 5300L))
expect_equal(Comma(c(0, 0.5, 1234.56), digits = 2L), c("0.00", "0.50", "1,234.56"))

expect_error(character2integer(55), "must be type char")
expect_error(character2integer("5300", na.strings = 0), "must be character")

expect_error(Comma(5300.2, digits = .Machine$integer.max), "unlikely high value")

expect_equal(character2integer(c("-99", "5300"), na.strings = "-99"), c(NA, 5300L))
expect_equal(character2integer(c("-99", "-8", "-99", "-9", "5300"),
                               na.strings = "-99"),
             c(NA, -8L, NA, -9L, 5300L))
expect_equal(character2integer(c("-99", "-8", "-99", "-9", "5300",
                                 "3,000,000,000"),
                               na.strings = "-99",
                               allow.double = TRUE),
             c(NA, -8L, NA, -9L, 5300L, 3e9))
expect_equal(character2integer(c("-99", "-8", "-99", "-9", "5300",
                                 "3,000,000,000"),
                               na.strings = c("-99", "-9"),
                               allow.double = TRUE),
             c(NA, -8L, NA, NA, 5300L, 3e9))
expect_equal(Comma(c(0.00000, 55), digits = 1L), c("0.0", "55.0"))
expect_equal(Comma(c(0.000001, 55), digits = 1L), c("0.0", "55.0"))
expect_equal(Comma(c(1.000001, 55), digits = 1L), c("1.0", "55.0"))
expect_equal(Comma(c(5123L), big.mark = " "), "5 123")
expect_equal(Comma(c(5123L), big.mark = '"'), '5"123')
expect_equal(Comma(c(5, 4, 5.5), big.mark = ",", digits = 1L),
             c("5.0", "4.0", "5.5"))
expect_equal(character2integer(c("5.0", "4.0", "5.5"), allow.double = TRUE,
                               na.strings = "na"),
             c(5, 4, 5.5))
expect_error(character2integer(c("5", "55", "55.005"), allow_double = FALSE))
