#' Every integer
#' @param nThread Number of threads.
#' @param na Value for \code{NA_INTEGER}.
#' @export

every_int <- function(nThread = getOption("hutilsc.nThread", 1L), na = NA_integer_) {
  .Call("Cevery_int32", nThread, na, PACKAGE = "hutilscpp") # nocov
}


