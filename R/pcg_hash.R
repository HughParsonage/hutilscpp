#
#
# pcg_hash <- function(n, r = NULL, nThread = getOption("hutilscpp.nThread", 1L)) {
#   if (is.null(r)) {
#     RS <- .Random.seed
#     r <- sample(RS, size = 33, replace = TRUE)
#   }
#   .Call("Cpcg_hash", n, r, check_omp(nThread), PACKAGE = packageName)
# }
#
#
# first_absent_int <- function(x, r = NA_integer_, nThread = getOption("hutilscpp.nThread", 1L)) {
#   .Call("firstAbsentInt", x, r, check_omp(nThread), PACKAGE = packageName)
# }
# first_absent_intbuf <- function(x, r = NA_integer_, nThread = getOption("hutilscpp.nThread", 1L)) {
#   .Call("firstAbsentIntBuf", x, r, check_omp(nThread), PACKAGE = packageName)
# }
#
# Tabulate256 <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
#   .Call("CTabulate256", x, check_omp(nThread), PACKAGE = packageName)
# }
#
# OneTo1024 <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
#   .Call("COneTo1024", x, nThread, PACKAGE = packageName)
# }
#
# FLIP <- function(x, nThread = getOption("hutilscpp.nThread", 1L)) {
#   .Call("CNot", x, nThread, PACKAGE = packageName)
# }
