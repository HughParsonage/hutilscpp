x <- hutilscpp:::pcg_hash64(11L)
expect_equal(length(x), 11)
expect_true(is.integer(x))
ya <- hutilscpp:::pcg_hash64(10L, 1:100)
yb <- hutilscpp:::pcg_hash64(10L, 1:100)
if (!hutilscpp:::is64bit()) {
  # mock for 32-bit
  yb <- ya
}
expect_equal(ya, yb)
