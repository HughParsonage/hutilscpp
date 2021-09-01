
if (requireNamespace("tinytest", quietly=TRUE)) {
  test_that <- function(desc, code) {
    code
    invisible()
  }
  tinytest::test_package("hutilscpp")
}

