

seqN_by <- function(x) {
  stopifnot(is.integer(x))
  .Call("Ccumsum_reset_sorted_int", x, PACKAGE = packageName)
}

shift_by_sorted_int <- function(x, y, z, nThread = getOption("hutilscpp.nThread", 1L)) {
  .Call("C_shift_by_sorted_int", x, y, z, nThread, PACKAGE = packageName)
}
