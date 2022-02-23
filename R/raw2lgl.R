

raw2lgl <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
  .Call("Craw2lgl", x, nThread, PACKAGE = "hutilscpp")
}

lgl2raw <- function(x, na = 0L, nThread = getOption("hutilscpp.nThread", 1L)) {
  .Call("Clgl2raw", x, na, nThread, PACKAGE = "hutilscpp")
}
