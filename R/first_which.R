

first_which <- function(expr) {
  o <- which(expr)
  if (length(o)) {
    return(o[1])
  } else {
    return(0L)
  }
}




