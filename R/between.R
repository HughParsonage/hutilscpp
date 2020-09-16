
"%(between)%" <- function(x, y) {
  stopifnot(length(y) == 2L)
  a <- y[1L]
  b <- y[2L]
  and(x > a, x < b)
}

"%]between[%" <- function(x, y) {
  stopifnot(length(y) == 2L)
  a <- y[1L]
  b <- y[2L]
  or(x <= a, x >= b)
}


