# nocov start
.onUnload <- function (libpath) {
  library.dynam.unload("hutilscpp", libpath)
}
# nocov end

