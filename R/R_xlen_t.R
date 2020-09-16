
R_xlen_t <- function(x) {
  if (x <= 2147483647) {
    x <- as.integer(x)
  }
  x
}
