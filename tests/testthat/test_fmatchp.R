
test_that("fmatchp works", {
  skip_if_not_installed("withr")
  withr::with_seed(3, {
    x <- sample.int(1e5)
    y <- sample.int(99)
    oi <- as.integer(fmatchp(x, y, nomatch = 0L, nThread = 2L))
    expect_equal(oi, match(x, y, nomatch = 0L))
    z <- sample(99, size = 98)

    oj <- as.integer(fmatchp(x, z, nomatch = 0L, nThread = 2L))
    expect_equal(oj, match(x, z, nomatch = 0L))

  })
})

