library(tinytest)
library(hutilscpp)

x <- rep_len(letters, 1e6)
expect_false(all(and3s(x == 'a')))
