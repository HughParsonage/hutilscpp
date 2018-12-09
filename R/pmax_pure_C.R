pmax_pure_c <- function(x, a) {
  if (is.double(x) && is.double(a) && length(a) == 1) {
    .Call("do_c_pmax", x, a)
  } else {
    stop("Unable.")
  }
}
