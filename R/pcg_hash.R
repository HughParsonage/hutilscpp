

pcg_hash <- function(n, r = NULL, nThread = getOption("hutilscpp.nThread", 1L)) {
  if (is.null(r)) {
    RS <- .Random.seed
    r <- sample(RS, size = 33, replace = TRUE)
  }
  .Call("Cpcg_hash", n, r, check_omp(nThread), PACKAGE = packageName)
}


first_absent_int <- function(x, r = NA_integer_) {
  .Call("firstAbsentInt", x, r, PACKAGE = packageName)
}
