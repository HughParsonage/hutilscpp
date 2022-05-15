# nocov start
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("hutilscpp.nThread"))) {
    options("hutilscpp.nThread" = 1L)
  }
  if (is.double(nThread <- getOption("hutilscpp.nThread"))) {
    options("hutilscpp.nThread" = as.integer(nThread))
  }

  invisible(NULL)
}

.onUnload <- function(libpath) {
  library.dynam.unload("hutilscpp", libpath)
}
# nocov end

