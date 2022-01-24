


ModeC <- function(x) {
  .Call("C_Mode", x, PACKAGE = "hutilscpp")
}
