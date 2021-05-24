

Cop2M <- function(operator) {
  .Call("C_op2M", operator, PACKAGE = packageName())
}

M2op <- function(M) {
  c("!=", "==", ">=", "<=", ">", "<", "%in%", "%between%", "%(between)%",
    "%]between[%")[M]
}

