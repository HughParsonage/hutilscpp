
if (requireNamespace("tinytest", quietly = TRUE) &&
    requireNamespace("data.table", quietly = TRUE) &&
    requireNamespace("hutils", quietly = TRUE)) {
  if (startsWith(Sys.getenv("USERNAME"), "hugh")) {
    options(hutilscpp.nThread = 10)
  }
  if (requireNamespace("covr", quietly = TRUE) && covr::in_covr()) {
    options(hutilscpp.nThread = 1L)
  }
  tinytest::test_package("hutilscpp")
}

