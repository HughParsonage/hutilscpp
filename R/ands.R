

.and_raw <- function(x, y, nThread = getOption("hutilscpp.nThread", 1L)) {
  .Call("C_and_raw", x, y,  nThread,  PACKAGE = "hutilscpp")
}

.or_raw <- function(x, y, nThread = getOption("hutilscpp.nThread", 1L)) {
  .Call("C_or_raw", x, y, nThread,  PACKAGE = "hutilscpp")
}



