
is_altrep <- function(x) {
  .Call("Cis_altrep", x, PACKAGE = packageName())
}
