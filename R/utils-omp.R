
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
  diagnosis <-
    diagnose_omp(nThread,
                 msg_no_openmp = g("No OpenMP detected."),
                 msg_threads_neg = g("`nThread = {nThread}`, a negative number, but must be a positive integer."),
                 msg_unknown_issues =  g("`nThread` was not a positive integer but the problem with it could not be diagnosed."),
                 msg_too_many_threads = g("`nThread = {nThread}` which exceeds the number of threads available on this computer."))
  if (diagnosis[[1]]) {
    if (diagnosis[[2]]) {
      warning(diagnosis[[3]])
    } else {
      stop(diagnosis[[3]])
    }
  }
  as.integer(nThread)
}

