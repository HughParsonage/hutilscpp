# Scalar recycling must work in every position in and3, including its
# no-missing-values shortcut with an omitted third operand.
for (x in list(c(TRUE, FALSE), c(FALSE, TRUE))) {
  z <- rev(x)
  expect_identical(and3(x, TRUE, z), x & z)
  expect_identical(and3(x, TRUE, z, nas_absent = TRUE), x & z)
  expect_identical(and3(TRUE, x, z), x & z)
  expect_identical(and3(x, z, TRUE), x & z)
  expect_identical(and3(x, FALSE, z), logical(length(x)))
}
for (x in c(FALSE, TRUE)) {
  for (y in c(FALSE, TRUE)) {
    expect_identical(and3(x, y, nas_absent = TRUE), x & y)
  }
}

# Identical logical operands still need to honor scan direction and
# skip missing comparisons, which are not TRUE.
for (x in list(c(TRUE, TRUE), c(FALSE, TRUE), c(NA, TRUE, NA),
               c(NA, NA))) {
  y <- x
  for (reverse in c(FALSE, TRUE)) {
    expected <- which(x == y)
    expected <- if (!length(expected)) 0L else {
      if (reverse) tail(expected, 1L) else expected[1L]
    }
    expect_identical(which_first(x == y, reverse = reverse), expected)
    expect_identical(which_first(x <= y, reverse = reverse), expected)
    expect_identical(which_first(x >= y, reverse = reverse), expected)
    expect_identical(which_first(x != y, reverse = reverse), 0L)
  }
}

# Character vectors use the base fallback, which must honor asc too.
ascending <- c("a", "b", "c")
descending <- rev(ascending)
expect_true(is_sorted(ascending, asc = TRUE))
expect_false(is_sorted(ascending, asc = FALSE))
expect_true(is_sorted(descending, asc = FALSE))
expect_false(is_sorted(descending, asc = TRUE))
expect_true(is_sorted(ascending))
expect_true(is_sorted(descending))
for (asc in c(TRUE, FALSE, NA)) {
  expect_true(is_sorted(character(), asc = asc))
  expect_true(is_sorted("a", asc = asc))
  expect_true(is_sorted(c("b", "b"), asc = asc))
  expect_false(is_sorted(c("b", "a", "c"), asc = asc))
}
