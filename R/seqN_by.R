

seqN_by <- function(x) {
  stopifnot(is.integer(x))
  .Call("Ccumsum_reset_sorted_int", x, PACKAGE = packageName())
}

