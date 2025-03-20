
if (requireNamespace("tinytest", quietly = TRUE) &&
    requireNamespace("data.table", quietly = TRUE) &&
    requireNamespace("hutils", quietly = TRUE)) {
  if (startsWith(Sys.getenv("USERNAME"), "hugh")) {
    options(hutilscpp.nThread = 10)
  }
  if (requireNamespace("covr", quietly = TRUE) && covr::in_covr()) {
    options(hutilscpp.nThread = 1L)
  }
  library(data.table)
  library(hutils, warn.conflicts = FALSE)
  tinytest::test_package("hutilscpp", verbose = 0L)
}

