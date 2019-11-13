

which_first_unequal <- function(x, y, tol = sqrt(.Machine$double.eps)) {
  if (is.double(x) || is.double(y)) {
    return(do_which_1st_uneq_dbl_dbl(x, y, tol))
  }
  if (is.integer(x) && is.integer(y)) {
    return(do_which_1st_uneq_int_int(x, y))
  }
  which_first(x != y)
}



