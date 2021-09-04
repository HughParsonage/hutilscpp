
if (requireNamespace("tinytest", quietly = TRUE) &&
    requireNamespace("data.table", quietly = TRUE) &&
    requireNamespace("hutils", quietly = TRUE)) {
  if (startsWith(Sys.getenv("USERNAME"), "hugh")) {
    options(hutilscpp.nThread = 10)
  }
  tinytest::test_package("hutilscpp")
}

