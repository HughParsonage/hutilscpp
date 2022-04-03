library(hutilscpp)
expect_equal(hutilscpp:::sum_raw(as.raw(1:200)), sum(1:200))
expect_equal(hutilscpp:::sum_raw(1:200), sum(1:200))
# if exceeds INT_MAX
expect_equal(hutilscpp:::sum_raw(rep(as.integer(2^30), 100)), sum(rep(2^30, 100)))
