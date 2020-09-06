pmax_pure_c <- function(x, a) {
  if (is.double(x) && is.double(a) && length(a) == 1L) {
    return(.Call("do_c_pmax", x, a, -Inf))
  }
  if (is.integer(x) && is.integer(a)) {
    return(.Call("do_c_pmax_int", x, a))
  }

  pmax.int(x, a)

}
