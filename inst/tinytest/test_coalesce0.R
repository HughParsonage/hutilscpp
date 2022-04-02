library(hutilscpp)

library(data.table)
DT <- data.table(x = 1:6,
                 y = c(NA, 1:5))
expect_equal(COALESCE0(1:5), 1:5)
COALESCE0(DT)
expect_equal(DT$y, 0:5)
expect_equal(COALESCE0(list(x = 1, y = 2:3, z = c(NA, 0, 2))),
             list(x = 1, y = 2:3, z = c(0, 0, 2)))


.coalesce0 <- function(x) {
  x[is.na(x)] <- FALSE
  x
}
expect_equal(coalesce0(logical(3)), logical(3))
expect_equal(coalesce0(c(NA, logical(3))), logical(4))
expect_equal(coalesce0(integer(0)), integer(0))
expect_equal(coalesce0(NA_integer_), 0L)
expect_equal(coalesce0(1:2), 1:2)
expect_equal(coalesce0(integer(2)), integer(2))
expect_equal(coalesce0(integer(3)), integer(3))
expect_equal(coalesce0(c(1:6, NA)), .coalesce0(c(1:6, NA)))
expect_equal(coalesce0(c(1:253, NA, NA, NA, 1:253)), .coalesce0(c(1:253, NA, NA, NA, 1:253)))


x <- c(1L, -.Machine$integer.max, .Machine$integer.max, 0L)
expect_equal(coalesce0(x), .coalesce0(x))
y <- rep_len(x, 1001)
expect_equal(coalesce0(y), .coalesce0(y))
x <- c(NA, x)
expect_equal(coalesce0(x), .coalesce0(x))
x <- c(NA, x, NA)
x <- rep_len(x, 1001)
expect_equal(coalesce0(x), .coalesce0(x))
y <- rep_len(x, 1001)
expect_equal(coalesce0(y), .coalesce0(y))

x <- as.double(x)
expect_equal(coalesce0(x), .coalesce0(x))
y <- rep_len(x, 1001)
expect_equal(coalesce0(y), .coalesce0(y))
x <- c(NA, x)
expect_equal(coalesce0(x), .coalesce0(x))
x <- c(NA, x, NA)
x <- rep_len(x, 1001)
expect_equal(coalesce0(x), .coalesce0(x))
y <- rep_len(x, 1001)
expect_equal(coalesce0(y), .coalesce0(y))

x <- c(FALSE, TRUE)
hutilscpp:::uncoalesce0(x)
expect_equal(x, c(NA, TRUE))
x <- c(0L, 1L)
hutilscpp:::uncoalesce0(x)
expect_equal(x, c(NA, 1L))
x <- c(0, 1)
hutilscpp:::uncoalesce0(x)
expect_equal(x, c(NA, 1))

expect_equal(coalesce0(raw(500)), raw(500))
L <- list(x = 1:5, y = c(NA, 1:5))
expect_equal(coalesce0(L), L)
expect_equal(COALESCE0(L), lapply(L, .coalesce0))

# must be NaN
cx.r <- c(NaN, rpois(5, 1))
cx.i <- rpois(6, 1)
cx <- complex(real = cx.r, imaginary = cx.i)

expect_equal(coalesce0(cx), complex(real = coalesce0(cx.r), imaginary = cx.i))





