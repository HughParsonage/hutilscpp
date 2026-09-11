library(hutilscpp)

if (!requireNamespace("bench", quietly = TRUE)) {
  stop("Install the suggested package 'bench' to run this benchmark.")
}

bminmax_base <- function(x) {
  2^c(floor(log2(min(x))), ceiling(log2(max(x))))
}

minmax_bounds <- function(x, nThread) {
  endpoints <- minmax(x, nThread = nThread)
  2^c(floor(log2(endpoints[1L])), ceiling(log2(endpoints[2L])))
}

set.seed(47)
x_double <- runif(1e7, 2^-20, 2^20)
x_integer <- sample.int(.Machine$integer.max, 1e7, replace = TRUE)

cat("Runtime AVX-512F dispatch:", hutilscpp:::.bminmax_has_avx512(), "\n")
n_thread <- tryCatch(hutilscpp:::check_omp(60L), error = function(e) 1L)
cat("OpenMP comparison threads:", n_thread, "\n")

print(bench::mark(
  bminmax_1 = bminmax(x_double),
  bminmax_threaded = bminmax(x_double, nThread = n_thread),
  minmax_threaded = minmax_bounds(x_double, nThread = n_thread),
  portable = hutilscpp:::.bminmax_portable(x_double),
  base = bminmax_base(x_double),
  check = TRUE,
  min_iterations = 20
)[, c("expression", "min", "median", "itr/sec", "mem_alloc")])

print(bench::mark(
  bminmax_1 = bminmax(x_integer),
  bminmax_threaded = bminmax(x_integer, nThread = n_thread),
  minmax_threaded = minmax_bounds(x_integer, nThread = n_thread),
  portable = hutilscpp:::.bminmax_portable(x_integer),
  base = bminmax_base(x_integer),
  check = TRUE,
  min_iterations = 20
)[, c("expression", "min", "median", "itr/sec", "mem_alloc")])
