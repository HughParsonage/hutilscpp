# Benchmark for row_id_by_pattern(). Not part of the package build.
#
# Target: 50M rows x 500 columns in <= 5 s at 10 threads. A 50M x 500 integer
# table is 100 GB, so integer and double cases are timed at smaller sizes and
# extrapolated linearly (the kernel is one streaming pass over the columns).
#
# Data are drawn from the stats:: random number generators. "K patterns"
# tables draw K distinct pattern rows and assign rows to them with geometric
# (Zipf-like) frequencies; "independent" tables draw every column
# independently, so every row is a distinct pattern (the worst case for the
# hash tables).
#
# Note: objects compiled by devtools/pkgload (roxygen, load_all) are built
# with -O0. Install with `R CMD INSTALL --preclean .` before timing.
#
# Usage: Rscript benchmarks/row_id_by_pattern.R [nrow_scale]
#
# Results, 2026-09-14, i7-6800K (6 cores / 12 threads, quad-channel DDR4),
# gcc 13 -O2, R 4.6.1, OpenMP 10 threads. "50M x 500" is the linear
# extrapolation of the 10-thread time to 25e9 cells.
#
#   case                        size          patterns   1 thr   10 thr  GB/s   50M x 500
#   raw, 1000 patterns          10M x 500         1000   2.09s   0.45s   11.1     2.26s
#   raw, independent            10M x 500     10000000   3.39s   1.25s    4.0     6.25s
#   int, 1000 patterns          10M x 500         1000   2.88s   0.66s   30.4     3.29s
#   int, 1000 patterns, mag     10M x 500         1000   7.99s   1.46s   13.7     7.32s
#   int, independent             2M x 500      2000000   0.85s   0.28s   14.5     6.88s
#   int, independent, mag        2M x 500      2000000   1.84s   0.43s    9.3    10.77s
#   double, 1000 patterns       10M x 100         1000   0.95s   0.23s   35.1     5.70s
#   double, 1000 patterns, mag  10M x 100         1000   4.29s   0.70s   11.5    17.40s
#   logical, 1000 patterns      10M x 500         1000   2.92s   0.65s   30.9     3.24s
#   mixed int/factor/char       10M x 500         1000   5.16s   0.99s   22.2     4.95s
#   mixed, 100k patterns        10M x 500        99998   5.62s   1.02s   21.5     5.12s
#
#   1M x 20 int (1000 patterns): row_id_by_pattern 0.008s; data.table
#   coalesce0 0.031s + .GRP 0.318s.
#
# The zero/nonzero pass is memory-bandwidth bound on integer, logical, and
# double columns (~30 GB/s here); the per-row cost (fold + hash insert) is
# 10-20 ns per row at 10 threads. Tables where every row is distinct pay
# for the hash tables (~2x). magnitude = TRUE is compute bound at about
# 3 Gcell/s (int) and 1.5 Gcell/s (double) on 10 threads.

library(hutilscpp)
library(data.table)

args <- commandArgs(trailingOnly = TRUE)
scale <- if (length(args)) as.numeric(args[1L]) else 1

n_thread <- tryCatch(hutilscpp:::check_omp(10L), error = function(e) 1L)
cat("OpenMP threads:", n_thread, "\n")

time_it <- function(expr, reps = 3L) {
  # Re-evaluate the expression on every repetition (a promise is cached).
  e <- substitute(expr)
  env <- parent.frame()
  times <- vapply(seq_len(reps), function(i) system.time(eval(e, env))[["elapsed"]], 0)
  min(times)
}

report <- function(label, DT, nThread, reps = 3L, ...) {
  n <- nrow(DT)
  k <- ncol(DT)
  cells <- as.double(n) * k
  bytes <- sum(vapply(DT, function(x) as.double(object.size(x)), 0))
  dots <- list(...)
  t1 <- time_it(do.call(row_id_by_pattern, c(list(DT, nThread = 1L), dots)), reps)
  tn <- time_it(do.call(row_id_by_pattern, c(list(DT, nThread = nThread), dots)), reps)
  ids <- do.call(row_id_by_pattern, c(list(DT, nThread = nThread), dots))
  cat(sprintf("%-30s %6.2fM x %3d %8d patterns  1 thr: %6.2fs  %2d thr: %6.2fs  %5.2f Gcell/s  %5.1f GB/s  50M x 500: %5.2fs\n",
              label, n / 1e6, k, max(ids), t1, nThread, tn, cells / tn / 1e9, bytes / tn / 1e9,
              tn * (50e6 * 500) / cells))
  invisible(NULL)
}

# Rows assigned to K patterns with geometric frequencies.
pattern_index <- function(n, K) {
  p <- dgeom(seq_len(K) - 1L, prob = 4 / K)
  sample.int(K, n, replace = TRUE, prob = p)
}

# One random column of a given type; `gen` draws n values from stats::.
rgen <- list(
  raw = function(n) as.raw(rbinom(n, 1L, runif(1, 0.05, 0.95)) * sample(1:255, 1L)),
  int01 = function(n) rbinom(n, 1L, runif(1, 0.05, 0.95)),
  int = function(n) {
    x <- switch(sample(4L, 1L),
                rpois(n, rexp(1, 0.5)),
                rgeom(n, runif(1, 0.1, 0.9)),
                rnbinom(n, size = 2, mu = rexp(1)),
                rbinom(n, 10L, runif(1)) - 5L)
    x[rbinom(n, 1L, 0.02) == 1L] <- NA
    as.integer(x)
  },
  lgl = function(n) rbinom(n, 1L, runif(1, 0.1, 0.9)) == 1L,
  dbl = function(n) {
    x <- switch(sample(6L, 1L),
                rnorm(n, 0, 10^runif(1, -3, 3)),
                rlnorm(n, runif(1, -5, 5), 3),
                rexp(n, 10^runif(1, -3, 3)),
                rcauchy(n),
                runif(n, -1, 1) * rbinom(n, 1L, 0.7),
                round(rnorm(n)))
    x[rbinom(n, 1L, 0.01) == 1L] <- NaN
    x
  },
  fct = function(n) factor(sample(letters[1:5], n, TRUE, prob = rexp(5))),
  chr = function(n) {
    x <- sample(c(month.name, NA), n, TRUE, prob = rexp(13))
    x
  }
)

# Draw K pattern rows for `types` and expand them to n rows.
patterned <- function(n, types, K) {
  idx <- pattern_index(n, K)
  as.data.table(lapply(types, function(ty) rgen[[ty]](K)[idx]))
}
independent <- function(n, types) {
  as.data.table(lapply(types, function(ty) rgen[[ty]](n)))
}

set.seed(20260914)

# (a) raw 10M x 500, 1000 patterns (5 GB)
n <- 10e6 * scale
DT <- patterned(n, rep("raw", 500), 1000L)
report("raw, 1000 patterns", DT, n_thread)
rm(DT); gc()

# (b) raw 10M x 500, independent (all rows distinct)
DT <- independent(n, rep("raw", 500))
report("raw, independent", DT, n_thread)
rm(DT); gc()

# (c) int 10M x 500, 1000 patterns (20 GB)
DT <- patterned(n, rep(c("int01", "int"), 250), 1000L)
report("int, 1000 patterns", DT, n_thread)
report("int, 1000 patterns, mag", DT, n_thread, magnitude = TRUE)
rm(DT); gc()

# (d) int 2M x 500, independent (all rows distinct, 4 GB)
DT <- independent(2e6 * scale, rep(c("int01", "int"), 250))
report("int, independent", DT, n_thread)
report("int, independent, mag", DT, n_thread, magnitude = TRUE)
rm(DT); gc()

# (e) double 10M x 100, 1000 patterns (8 GB)
DT <- patterned(n, rep("dbl", 100), 1000L)
report("double, 1000 patterns", DT, n_thread)
report("double, 1000 patterns, mag", DT, n_thread, magnitude = TRUE)
rm(DT); gc()

# (f) logical 10M x 500, 1000 patterns
DT <- patterned(n, rep("lgl", 500), 1000L)
report("logical, 1000 patterns", DT, n_thread)
rm(DT); gc()

# (g) mixed 10M x (400 int01 + 50 factor + 50 character), 1000 patterns
DT <- patterned(n, c(rep("int01", 400), rep("fct", 50), rep("chr", 50)), 1000L)
report("mixed int/factor/char", DT, n_thread)
rm(DT); gc()

# (h) mixed, 100k patterns
DT <- patterned(n, c(rep("int01", 400), rep("fct", 50), rep("chr", 50)), 100000L)
report("mixed, 100k patterns", DT, n_thread)
rm(DT); gc()

# Correctness cross-check and comparison against data.table at 1M rows.
# The data.table route needs the patterns as real columns first: one in-place
# coalesce0() pass on a copy (0/1 columns here are already 0/1 so no further
# transform is needed), then a single .GRP grouping. The two stages are timed
# separately; neither ranks groups by frequency as row_id_by_pattern does.
n <- 1e6
DT <- patterned(n, rep("int01", 20), 1000L)
for (j in seq_along(DT)) set(DT, i = which(rbinom(n, 1L, 0.01) == 1L), j = j, value = NA_integer_)
cols <- copy(names(DT)) # names(DT) is modified by reference by := below
ids <- row_id_by_pattern(DT, nThread = n_thread)
P <- COALESCE0(copy(DT), nThread = n_thread)
P[, first := .I]
G <- P[, .(n = .N, first = min(first)), by = cols]
setorder(G, -n, first)
G[, id := .I]
ref <- G[P, on = cols]$id
cat("Matches data.table reference at 1M x 20:", identical(ids, ref), "\n")
cat(sprintf("row_id_by_pattern, 1M x 20, %d threads: %.3fs\n", n_thread,
            time_it(row_id_by_pattern(DT, nThread = n_thread))))
cat(sprintf("data.table: coalesce0 in place on a copy: %.3fs; .GRP grouping: %.3fs\n",
            time_it(COALESCE0(copy(DT), nThread = n_thread)),
            time_it(P[, .GRP, by = cols])))
