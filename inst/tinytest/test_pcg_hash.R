x <- hutilscpp:::lehmer64(11L)
expect_equal(length(x), 11)
expect_true(is.integer(x))
ya <- hutilscpp:::lehmer64(10L, 1:100)
yb <- hutilscpp:::lehmer64(10L, 1:100)
if (!hutilscpp:::is64bit()) {
  # mock for 32-bit
  yb <- ya
}
expect_equal(ya, yb)
pcg_hash <- hutilscpp:::pcg_hash
x <- hutilscpp:::pcg_hash(105L)
expect_equal(length(x), 105)
x <- hutilscpp:::pcg_hash(105, raw_result = TRUE)
expect_true(is.raw(x))
if (tinytest::at_home()) {
  expect_equal(length(pcg_hash(106, nThread = 2L)), 106)
  expect_equal(length(pcg_hash(106, nThread = 52L)), 106)
  expect_equal(pcg_hash(106, r = 1:200), pcg_hash(106, r = 1:200))
}


