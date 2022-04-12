library(hutilscpp)
expect_equal(diam(c(0L, 1L)), 1L)
expect_true(thinner(0:5, 8L))
expect_false(thinner(0:5, 2L))
