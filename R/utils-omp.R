
check_omp <- function(nThread) {
  if (identical(nThread, 1L) || identical(nThread, 1)) {
    return(1L)
  }
  if (res <- isnt_number(nThread, int.only = TRUE)) {
    stop(attr(res, "ErrorMessage"))
  }
  if (nThread != as.integer(nThread)) {
    stop("`nThread = ", nThread, "` was not a whole number.")
  }
  if (nThread <= 0) {
    stop("`nThread = ", nThread, "` but must be a positive whole number.")
  }
  omp_diagnosis_int <- .Call("Cdiagnose_omp", nThread, PACKAGE = packageName)
  if (omp_diagnosis_int == 2L) {
    stop("`nThread = ", nThread, "`, which exceeds the number of threads available on this computer.")
  }
  if (omp_diagnosis_int != 0L) {
    stop("`nThread = ", nThread, "` could not be used.")
  }
                      # Coverage not needed since normally nothing has changed
  as.integer(nThread) # nocov
}

has_openmp <- function() {
  # Use NULL because SEXP C functions cannot accept void
  .Call("Chas_openmp", NULL, PACKAGE = packageName)
}
