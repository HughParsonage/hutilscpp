library(hutilscpp)

x <- c(.Machine$integer.max, -.Machine$integer.max, 0L, 0L)
expect_equal(unique_fmatch(x, nThread = 1L), x[1:3])
expect_equal(uniqueN_fmatch(x), 3L)
