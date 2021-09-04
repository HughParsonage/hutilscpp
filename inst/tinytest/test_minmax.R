
z <- integer(0)
expect_true(is.null(minmax(z)))
expect_equal(minmax(z, empty_result = c(-Inf, Inf)), c(-Inf, Inf))

x <- logical(3)
expect_equal(minmax(x), c(FALSE, FALSE))
x <- !logical(3)
expect_equal(minmax(x), c(TRUE, TRUE))
x <- c(TRUE, FALSE, FALSE)
expect_equal(minmax(x), c(FALSE, TRUE))
x <- c(FALSE, TRUE)
expect_equal(minmax(x), c(FALSE, TRUE))
x <- sample(100)
expect_equal(minmax(x), c(1, 100))
x <- c(NaN, x)
expect_equal(minmax(x), c(1, 100))
x <- as.double(x)
expect_equal(minmax(x), c(1, 100))
x <- letters
expect_equal(minmax(x), c("a", "z"))


