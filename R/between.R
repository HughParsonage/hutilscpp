
"%(between)%" <- function(x, y) {
  stopifnot(length(y) == 2L, is.atomic(y))
  a <- y[1L]
  b <- y[2L]
  if (is.na(a) && is.na(b)) {
    return(rep(TRUE, length(x)))
  }
  if (is.na(a)) {
    return(x < b)
  }
  if (is.na(b)) {
    return(x > a)
  }
  and(x > a, x < b)
}

"%]between[%" <- function(x, y) {
  stopifnot(length(y) == 2L, is.atomic(y))
  a <- y[1L]
  b <- y[2L]
  if (is.na(a) && is.na(b)) {
    return(rep(TRUE, length(x)))
  }
  if (is.na(a)) {
    return(x >= b)
  }
  if (is.na(b)) {
    return(x <= a)
  }
  if (b < a) {
    return(logical(length(x)))
  }
  or(x <= a, x >= b)
}

# m is unsupported
Between <- function(x, a, b, m = 0L, nThread = getOption("hutilscpp.nThread", 1L)) {
  .Call("CBetween", x, a, b, m, nThread, PACKAGE = "hutilscpp")
}


