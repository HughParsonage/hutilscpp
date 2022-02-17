
sum_raw <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
  .Call("Csum_raw", x, nThread, PACKAGE = "hutilscpp")
}

