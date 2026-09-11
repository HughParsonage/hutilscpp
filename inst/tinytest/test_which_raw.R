
ans <- c(TRUE, FALSE, TRUE)
expect_equal(hutilscpp:::which_raw(as.raw(ans)), which(ans))


len <- if (hutilscpp:::is64bit()) 2^31 else 1
ans <- c(raw(len), as.raw(1), as.raw(0))
expect_equal(hutilscpp:::which_raw(ans), len + 1)
