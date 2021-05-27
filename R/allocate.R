

allocate0_int <- function(N, nThread = 1L) {
  if ((res <- isnt_number(N)) || (res <- isnt_number(nThread))) {
    stop(attr(res, "ErrorMessage")) # nocov
  }
  .Call("Callocate0_int", N, nThread, PACKAGE = packageName)
}

allocate0_dbl <- function(N, nThread = 1L) {
  if ((res <- isnt_number(N)) || (res <- isnt_number(nThread))) {
    stop(attr(res, "ErrorMessage")) # nocov
  }
  .Call("Callocate0_dbl", N, nThread, PACKAGE = packageName)
}


allocate0_except <- function(N, India, Victor, nThread = 1L) {
  if ((res <- isnt_number(N)) || (res <- isnt_number(nThread))) {
    stop(attr(res, "ErrorMessage")) # nocov
  }
  stopifnot(is.integer(India) || isTRUE(all.equal(floor(India), India)))
  Victor <- ensure_integer(Victor)
  nThread <- check_omp(nThread)
  .Call("Callocate0_except", N, India, Victor, nThread, PACKAGE = packageName)
}

allocate_with_root <- function(N, a, r, left, do_pmin, nThread = 1L) {
  .Call("Callocate_with_root", N, a, r, left, do_pmin, nThread, PACKAGE = packageName)
}

.allocate_constants <- function(N) {
  N <- as.integer(N)
  list(LogicalN = .Call("CallocateConstants", N, 0L, PACKAGE = packageName),
       IntegerN = .Call("CallocateConstants", N, 1L, PACKAGE = packageName),
       IntegerNNA = .Call("CallocateConstants", N, 2L, PACKAGE = packageName),
       DoubleN = .Call("CallocateConstants", N, 3L, PACKAGE = packageName),
       DoubleNNA = .Call("CallocateConstants", N, 4L, PACKAGE = packageName))
}
