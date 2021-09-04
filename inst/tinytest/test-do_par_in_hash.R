# test_that("do_par_hash works", {
  expect_equal(hutilscpp:::do_par_in_hash_int(1:200, 4:66), 1:200 %in% 4:66)
  expect_equal(hutilscpp:::do_par_in_hash_dbl(as.double(1:200), 4:66), 1:200 %in% 4:66)

