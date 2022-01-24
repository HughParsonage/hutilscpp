

Cop2M <- function(operator) {
  .Call("C_op2M", operator, PACKAGE = packageName)
}

M2op <- function(M) {
  c("!=", "==", ">=", "<=", ">", "<", "%in%", "%between%", "%(between)%",
    "%]between[%", "%notin%")[M]
}

op2M <- function(operator) {
  switch(operator,
         "!=" = 1L,
         "==" = 2L,
         ">=" = 3L,
         "<=" = 4L,
         ">"  = 5L,
         "<"  = 6L,
         "%in%" = 7L,
         "%between%" = 8L,
         "%(between)%" = 9L,
         "%]between[%" = 10L,
         "%notin%" = 11L,
         0L)
}
