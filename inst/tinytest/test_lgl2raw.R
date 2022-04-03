expect_equal(hutilscpp:::lgl2raw(c(TRUE, FALSE, NA)), as.raw(c(1L, 0L, 0L)))
expect_equal(hutilscpp:::lgl2raw(c(TRUE, FALSE, NA), na = 2),
             as.raw(c(1L, 0L, 2L)))
expect_error(hutilscpp:::raw2lgl(""), "must be raw")
