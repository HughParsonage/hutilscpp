

is_seq <- function(x) {
  .Call("Cis_seq", x, PACKAGE = "hutilscpp")
}

