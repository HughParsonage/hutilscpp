
check_omp <- function(nThread) {
  if (identical(nThread, 1L) || identical(nThread, 1)) {
    return(1L)
  }
  if (res <- isnt_number(nThread)) {
    stop(attr(res, "ErrorMessage"))
  }
  if (nThread != as.integer(nThread)) {
    stop("`nThread = ", nThread, "` was not a whole number.")
  }
  omp_diagnosis_int <- .Call("Cdiagnose_omp", nThread, PACKAGE = packageName)
  g <- paste0
  diagnosis <-
    switch(omp_diagnosis_int,
           g("No OpenMP detected."),
           g("`nThread = ", nThread, "`, a negative number, but must be a positive integer."),
           g("`nThread` was not a positive integer but the problem with it could not be diagnosed."),
           g("`nThread = ", nThread, "`, which exceeds the number of threads available on this computer."))
  if (omp_diagnosis_int) {
    stop(diagnosis)
  }
                      # Coverage not needed since normally nothing has changed
  as.integer(nThread) # nocov
}

has_openmp <- function() {
  # Use NULL because SEXP C functions cannot accept void
  .Call("Chas_openmp", NULL, PACKAGE = packageName)
}

