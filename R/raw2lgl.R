

raw2lgl <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
  .Call("Craw2lgl", x, nThread, PACKAGE = "hutilscpp")
}
