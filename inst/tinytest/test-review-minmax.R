# Missing values must survive each mixed-type dispatch direction, including
# the scalar branches used by one-element vector inputs.
for (fun in list(pminV, pmaxV)) {
  reference <- if (identical(fun, pminV)) pmin else pmax
  for (lhs in list(c(NA_integer_, 2L, 4L, NA_integer_),
                   c(1, NA_real_, 3.5, NA_real_))) {
    rhs <- if (is.integer(lhs)) c(1, NA_real_, 3.5, NA_real_) else c(NA_integer_, 2L, 4L, NA_integer_)
    for (in_place in c(FALSE, TRUE)) {
      x <- lhs + 0L
      expected <- reference(x, rhs)
      result <- fun(x, rhs, in_place = in_place)
      expect_equal(result, expected)
      if (in_place) expect_equal(x, expected)
    }
  }
  for (lhs in list(NA_integer_, 1L, NA_real_, 1)) {
    for (rhs in list(NA_integer_, 1L, NA_real_, 1)) {
      expect_equal(fun(lhs, rhs), reference(lhs, rhs))
    }
  }
}

for (fun in list(pminC, pmaxC)) {
  reference <- if (identical(fun, pminC)) pmin else pmax
  for (a in list(2L, 2, 2.5, -3e9)) {
    for (in_place in c(FALSE, TRUE)) {
      x <- c(NA_integer_, -1L, 1L, 4L)
      expected <- reference(x, a)
      result <- fun(x, a, in_place = in_place, keep_nas = TRUE, dbl_ok = TRUE)
      expect_equal(result, expected)
      # Promotion allocates a double result; representable bounds can mutate x.
      if (in_place && a %in% c(2L, 2)) expect_equal(x, expected)
    }
  }
  # keep_nas = FALSE retains the documented native-representation behavior.
  x <- c(NA_integer_, 1L, 4L)
  expected <- fun(x, 2L, keep_nas = FALSE)
  result <- fun(x, 2L, in_place = TRUE, keep_nas = FALSE)
  expect_identical(result, expected)
  expect_identical(x, expected)
}

x <- c(NA_integer_, -1L, 1L)
expect_identical(pmax0(x, in_place = TRUE, keep_nas = TRUE), c(NA_integer_, 0L, 1L))
expect_identical(x, c(NA_integer_, 0L, 1L))
