library(hutilscpp)

# Sequence membership is discrete, including descending and fractional tables.
for (table in list(1:3, 3:1, as.double(1:3), c(NA_integer_, 1L),
                   c(NA_integer_, NA_integer_), (1:3) + 0.5)) {
  for (x in list(c(1.5, 2, NA_real_, NaN, Inf), c(1L, 2L, NA_integer_))) {
    expect_identical(finp(x, table), x %in% table)
  }
}
expect_identical(finp(factor(c("a", "b")), 1:2), c(FALSE, FALSE))

# Logical queries must retain match() coercion, missingness, and result types.
x <- c(FALSE, TRUE, NA)
for (table in list(logical(), FALSE, TRUE, NA, c(FALSE, TRUE),
                   c(TRUE, NA), c(FALSE, TRUE, NA), c(TRUE, FALSE, TRUE),
                   2L, c(0L, 2L, NA_integer_), c("TRUE", "NA"))) {
  for (nomatch in list(NA_integer_, 0L, 99L)) {
    expect_identical(fmatchp(x, table, nomatch = nomatch), match(x, table, nomatch = nomatch))
  }
  expect_identical(finp(x, table), x %in% table)
  expect_identical(fnotinp(x, table), !(x %in% table))
  matches <- which(x %in% table)
  first <- if (length(matches)) matches[1L] else 0L
  last <- if (length(matches)) tail(matches, 1L) else 0L
  expect_identical(fmatchp(x, table, whichFirst = 1L), first)
  expect_identical(fmatchp(x, table, whichFirst = -1L), last)
}
expect_identical(fmatchp(logical(), TRUE), integer())

# Integer remainder must preserve signs and propagate missing values.
x <- c(-.Machine$integer.max, -9:9, .Machine$integer.max, NA_integer_)
for (d in c(-7L, -3L, -1L, 1L, 3L, 7L, NA_integer_)) {
  expect_identical(divisible(x, d), x %% d == 0L)
}
expect_error(divisible(1L, integer()), "length one")
expect_error(divisible(1L, c(1L, 2L)), "length one")

# which.max returns the first winning index, including scalar recycling.
expect_identical(abs_diff(c(4, 3, 2), c(0, 0, 0), option = 3L), 1L)
expect_identical(abs_diff(c(4, 3, 2), 0.5, option = 3L), 1L)
expect_identical(abs_diff(0.5, c(4, 3, 2), option = 3L), 1L)
expect_identical(abs_diff(c(4, 4, 2), c(0, 0, 0), option = 3L, nThread = 1L), 1L)
for (x in list(integer(), double())) {
  expect_identical(abs_diff(x, 1L, option = 3L), integer())
  expect_identical(abs_diff(1L, x, option = 3L), integer())
}
x <- c(NA_real_, 3, 2)
y <- c(0, 0, 0)
expect_identical(abs_diff(x, y, option = 3L), which.max(abs(x - y)))
expect_identical(abs_diff(NA_real_, NA_real_, option = 3L), integer())
x <- c(3, 4, 2, 1)
y <- c(1, 2)
expect_identical(abs_diff(x, y, option = 3L), which.max(abs(x - y)))
