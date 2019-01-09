pmax_pure_c <- function(x, a) {
  if (is.double(x) && is.double(a) && length(a) == 1L) {
    .Call("do_c_pmax", x, a, -Inf)
  } else {
    pmax.int(x, a)
  }
}
