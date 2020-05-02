
check_omp <- function(nThread) {
  if (res <- isnt_number(nThread)) {
    stop(attr(res, "ErrorMessage"))
  }
  if (which_isnt_integerish(nThread)) {
    stop("`nThread = ", nThread, "` was not a whole number.")
  }
  diagnosis <- diagnose_omp(nThread, "", "", "", "")
  if (diagnosis[[1]]) {
    if (diagnosis[[2]]) {
      warning(diagnosis[[3]])
    } else {
      stop(diagnosis[[3]])
    }
  }
  as.integer(nThread)
}

