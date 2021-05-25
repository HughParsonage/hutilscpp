
test_that("fmatchp works", {
  expect_true(TRUE)
  skip_if_not_installed("withr")
  withr::with_seed(3, {
    x <- sample.int(1e5)
    y <- sample.int(99)
    oi <- as.integer(fmatchp(x, y, nomatch = 0L, nThread = 2L))
    expect_equal(oi, match(x, y, nomatch = 0L))
    z <- sample(99, size = 98)

    oj <- as.integer(fmatchp(x, z, nomatch = 0L, nThread = 2L))
    expect_equal(oj, match(x, z, nomatch = 0L))
    expect_equal(oj, match(as.double(x), z, nomatch = 0L))
    expect_equal(oj, match(as.double(x), as.double(z), nomatch = 0L))

    z <- "z"
    lettre <- c(letters, "e")
    expect_equal(fmatchp(z, lettre), match(z, lettre))
    expect_equal(finp(z, lettre), z %in% lettre)
    expect_equal(finp(letters, lettre), letters %in% lettre)

  })


  # if (is_covr() && Sys.getenv("USERNAME") == "hughp") {
  #   x0 <- sample(2:100)
  #   x <- rep_len(x0, 2^31)
  #   y <- rep_len(sample(55), 13333)
  #   expect_identical(finp(x, y, nThread = 10L), rep_len(x0 %in% y, 2^31))
  #   expect_identical(finp(as.double(x), y, nThread = 10L)[1:101], rep_len(x0 %in% y, 101))
  #   expect_identical(finp(as.character(x), as.character(y), nThread = 10L)[1:101], rep_len(x0 %in% y, 101))
  # }

})

