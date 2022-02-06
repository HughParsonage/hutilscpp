

uminus <- function(x, y) {
  .Call("Cuminus", as.integer(x), as.integer(y), PACKAGE = "hutilscpp");
}
