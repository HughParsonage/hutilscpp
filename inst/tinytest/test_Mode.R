
a <- sample(100L, size = 1L)
x <- c(a, sample(100L))
expect_equal(ModeC(x), a)

a <- sample(1000L, size = 1L)
x <- c(a, sample(1000L))
expect_equal(ModeC(x), a)

x <- c(-.Machine$integer.max, x, .Machine$integer.max)
expect_equal(ModeC(x), a)

x <- c(NA, x)
expect_equal(ModeC(x), a)

x <- c(-.Machine$integer.max, .Machine$integer.max,
       8L, 1:1e5)
expect_equal(ModeC(x), 8L)
x <- as.double(x)
expect_equal(ModeC(x), 8)


