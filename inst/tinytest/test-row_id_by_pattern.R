library(hutilscpp)
library(data.table)

# Reference implementation --------------------------------------------------

exact_log2_floor <- function(x) {
  # floor(log2(abs(x))) without rounding error at powers of two
  a <- abs(x)
  e <- floor(log2(a))
  e <- e + (2^(e + 1) <= a) - (2^e > a)
  e
}

pattern_col <- function(x, na_is, magnitude) {
  if (is.factor(x)) {
    return(as.integer(x))
  }
  switch(typeof(x),
         character = x,
         raw = as.integer(x != as.raw(0)),
         logical = fifelse(is.na(x), na_is, as.integer(x)),
         integer = ,
         double = {
           x <- unclass(x)
           if (magnitude) {
             out <- character(length(x))
             isna <- is.na(x)
             iszero <- !isna & x == 0
             isinf <- !isna & is.infinite(x)
             out[iszero] <- "0"
             out[isna] <- if (na_is) "NA" else "0"
             out[isinf] <- paste0("Inf", sign(x[isinf]))
             rest <- !(isna | iszero | isinf)
             out[rest] <- paste(sign(x[rest]), exact_log2_floor(x[rest]))
             out
           } else {
             fifelse(is.na(x), na_is, as.integer(x != 0))
           }
         },
         stop("unsupported"))
}

ref_row_id <- function(DT, na_is = 0L, magnitude = FALSE, max_patterns = .Machine$integer.max) {
  if (length(DT) == 0L) {
    return(rep(1L, nrow(DT)))
  }
  P <- as.data.table(lapply(DT, pattern_col, na_is = na_is, magnitude = magnitude))
  setnames(P, paste0("V", seq_along(P)))
  cols <- copy(names(P)) # names(P) is modified by reference below
  P[, first_row__ := .I]
  G <- P[, .(n__ = .N, first__ = min(first_row__)), by = cols]
  setorder(G, -n__, first__)
  G[, id__ := .I]
  G[id__ > max_patterns, id__ := NA_integer_]
  G[P, on = cols][["id__"]]
}

# Basic behaviour -----------------------------------------------------------

DT <- data.frame(a = c(0, 1, 2, 0), b = c("x", "y", "y", "x"))
expect_identical(row_id_by_pattern(DT), c(1L, 2L, 2L, 1L))
expect_identical(row_id_by_pattern(DT, max_patterns = 1L), c(1L, NA, NA, 1L))
expect_identical(row_id_by_pattern(data.table(x = c(5L, 0L, 5L))), c(1L, 2L, 1L))
# Ties are broken by first appearance
expect_identical(row_id_by_pattern(data.table(x = c(0L, 5L))), c(1L, 2L))
expect_identical(row_id_by_pattern(data.table(x = c(5L, 0L))), c(1L, 2L))

# Shapes
expect_identical(row_id_by_pattern(data.table(x = integer(0))), integer(0))
expect_identical(row_id_by_pattern(data.table(x = 3L)), 1L)
expect_identical(row_id_by_pattern(data.frame(row.names = 1:3)), rep(1L, 3L))
expect_identical(row_id_by_pattern(data.frame()), integer(0))
# ALTREP column
expect_identical(row_id_by_pattern(data.table(x = 1:10)), rep(1L, 10L))
expect_identical(row_id_by_pattern(data.table(x = 0:9)), c(2L, rep(1L, 9L)))

# NA handling -----------------------------------------------------------------

DTna <- data.table(i = c(NA, 0L, 1L), d = c(NaN, NA, 0), l = c(NA, TRUE, FALSE))
expect_identical(row_id_by_pattern(DTna, na_is = 0L), ref_row_id(DTna, 0L))
expect_identical(row_id_by_pattern(DTna, na_is = 1L), ref_row_id(DTna, 1L))
expect_identical(row_id_by_pattern(data.table(i = c(NA, 0L)), na_is = 0L), c(1L, 1L))
expect_identical(row_id_by_pattern(data.table(i = c(NA, 0L)), na_is = 1L), c(1L, 2L))
expect_identical(row_id_by_pattern(data.table(d = c(NA, NaN, 0, -0)), na_is = 0), rep(1L, 4L))
expect_identical(row_id_by_pattern(data.table(d = c(NA, NaN, 1)), na_is = 1), rep(1L, 3L))
# Character NA is its own value regardless of na_is
expect_identical(row_id_by_pattern(data.table(s = c(NA, "", "a")), na_is = 0L), 1:3)
expect_identical(row_id_by_pattern(data.table(s = c(NA, "", "a")), na_is = 1L), 1:3)
# Factor NA
f <- factor(c("a", NA, "b", "a"))
expect_identical(row_id_by_pattern(data.table(f = f)), c(1L, 2L, 3L, 1L))
f2 <- addNA(f)
expect_identical(row_id_by_pattern(data.table(f = f2)), c(1L, 2L, 3L, 1L))
# Factors are grouped by level, not by zero/nonzero of the code
expect_identical(row_id_by_pattern(data.table(f = factor(c("a", "b", "c")))), 1:3)

# Classed numeric columns follow storage type
expect_identical(row_id_by_pattern(data.table(d = as.Date(c(0, 1, 2), origin = "1970-01-01"))), c(2L, 1L, 1L))
expect_identical(row_id_by_pattern(data.table(d = as.IDate(c(0L, 1L)))), 1:2)
expect_identical(row_id_by_pattern(data.table(t = as.POSIXct(c(0, 5), origin = "1970-01-01", tz = "UTC"))), 1:2)

# Randomised mixed-type tables against the reference -------------------------

make_mixed <- function(n, seed) {
  set.seed(seed)
  data.table(i = sample(c(0L, 1L, NA, 7L), n, TRUE),
             d = sample(c(0, 2.5, NaN, NA, -0), n, TRUE),
             l = sample(c(TRUE, FALSE, NA), n, TRUE),
             r = as.raw(sample(0:2, n, TRUE)),
             f = factor(sample(c("a", "b", NA), n, TRUE)),
             s = sample(c("p", "q", NA, ""), n, TRUE))
}
for (n in c(2047L, 2048L, 2049L, 4097L, 8193L, 20000L)) {
  D <- make_mixed(n, n)
  for (na in 0:1) {
    expect_identical(row_id_by_pattern(D, na_is = na), ref_row_id(D, na), info = paste(n, na))
    expect_identical(row_id_by_pattern(D, na_is = na, magnitude = TRUE),
                     ref_row_id(D, na, magnitude = TRUE), info = paste("mag", n, na))
  }
}

# Ordering properties on a random table
D <- make_mixed(30000L, 99L)
ids <- row_id_by_pattern(D)
tab <- tabulate(ids)
expect_true(all(diff(tab) <= 0))
expect_identical(sort(unique(ids)), seq_len(max(ids)))
first_seen <- match(seq_len(max(ids)), ids)
# Within equal counts, first appearance increases with id
for (ct in unique(tab)) {
  w <- which(tab == ct)
  expect_true(!is.unsorted(first_seen[w]))
}

# max_patterns ---------------------------------------------------------------

D <- make_mixed(5000L, 7L)
full <- row_id_by_pattern(D)
G <- max(full)
for (k in c(1L, 2L, G - 1L, G, G + 1L, 1e6)) {
  expect_identical(row_id_by_pattern(D, max_patterns = k), ref_row_id(D, max_patterns = k), info = k)
  capped <- full
  capped[capped > k] <- NA_integer_
  expect_identical(row_id_by_pattern(D, max_patterns = k), capped, info = k)
}
# Tie at the boundary resolved by first appearance
Dt <- data.table(x = c(1L, 0L, 1L, 0L))
expect_identical(row_id_by_pattern(Dt, max_patterns = 1L), c(1L, NA, 1L, NA))
expect_identical(row_id_by_pattern(Dt, max_patterns = 2^31 - 1), c(1L, 2L, 1L, 2L))

# Fold schedule: many one-bit columns and mixtures around the 64-bit word ----

wide01 <- function(n, k, seed) {
  set.seed(seed)
  as.data.table(replicate(k, sample(c(0L, 1L), n, TRUE), simplify = FALSE))
}
for (k in c(1L, 63L, 64L, 65L, 127L, 128L, 129L, 200L)) {
  D <- wide01(3000L, k, k)
  expect_identical(row_id_by_pattern(D), ref_row_id(D), info = k)
}
# All 2^k patterns present and distinct for small k
D <- as.data.table(expand.grid(rep(list(0:1), 6)))
expect_identical(length(unique(row_id_by_pattern(D))), 64L)
# 60 one-bit columns then a character / factor / magnitude-double column
D <- wide01(3000L, 60L, 60L)
set.seed(3)
D[, s := sample(c("a", "b", "c"), .N, TRUE)]
D[, f := factor(sample(c("x", "y"), .N, TRUE))]
D[, m := sample(c(0, 1, 2, 4, -4, 1e300), .N, TRUE)]
expect_identical(row_id_by_pattern(D), ref_row_id(D))
expect_identical(row_id_by_pattern(D, magnitude = TRUE), ref_row_id(D, magnitude = TRUE))
# All-distinct rows
D <- data.table(s = as.character(1:10000))
expect_identical(row_id_by_pattern(D), 1:10000)
D <- data.table(i = 1:10000)
expect_identical(row_id_by_pattern(D, magnitude = TRUE), ref_row_id(D, magnitude = TRUE))

# Magnitude ------------------------------------------------------------------

smallest <- 2^-1074
ks <- c(-1074L, -1073L, -1023L, -1022L, -1021L, -10L, 0L, 10L, 1023L)
vals <- c(0, -0, 1, -1, smallest, -smallest, 3 * smallest,
          .Machine$double.xmax, -.Machine$double.xmax, Inf, -Inf, NaN, NA_real_,
          0.5, 0.75, 1.5, 2, 3, 4, -2, -3, -4, 1e300, -1e-300)
for (k in ks) {
  p <- 2^k
  vals <- c(vals, p, -p, p * (1 + .Machine$double.eps))
  if (k > -1074L) vals <- c(vals, p - smallest)
  if (k > -1022L) vals <- c(vals, p * (1 - .Machine$double.eps / 2))
}
D <- data.table(x = vals)
for (na in 0:1) {
  expect_identical(row_id_by_pattern(D, na_is = na, magnitude = TRUE),
                   ref_row_id(D, na, magnitude = TRUE), info = na)
}
# Same magnitude bucket iff same sign and floor(log2)
expect_identical(row_id_by_pattern(data.table(x = c(2, 3, 3.99, 4)), magnitude = TRUE), c(1L, 1L, 1L, 2L))
expect_identical(row_id_by_pattern(data.table(x = c(2, -2, 0, -0)), magnitude = TRUE), c(2L, 3L, 1L, 1L))
expect_identical(row_id_by_pattern(data.table(x = c(NA, 0, NaN)), magnitude = TRUE, na_is = 0L), rep(1L, 3L))
expect_identical(row_id_by_pattern(data.table(x = c(NA, 0, NaN)), magnitude = TRUE, na_is = 1L), c(1L, 2L, 1L))
expect_identical(row_id_by_pattern(data.table(x = c(Inf, -Inf, 1e308)), magnitude = TRUE), 1:3)
# Integer magnitude
ivals <- c(0L, 1L, -1L, 2L, 3L, -2L, -3L, 4L, 7L, 8L, NA_integer_,
           .Machine$integer.max, -.Machine$integer.max, 2L^(0:30), -(2L^(0:30)), 2L^(1:30) - 1L)
D <- data.table(x = ivals)
for (na in 0:1) {
  expect_identical(row_id_by_pattern(D, na_is = na, magnitude = TRUE),
                   ref_row_id(D, na, magnitude = TRUE), info = na)
}
expect_identical(row_id_by_pattern(data.table(x = c(NA_integer_, 0L)), magnitude = TRUE, na_is = 0L), c(1L, 1L))
expect_identical(row_id_by_pattern(data.table(x = c(NA_integer_, 0L)), magnitude = TRUE, na_is = 1L), 1:2)
# Logical and raw are unaffected by magnitude
D <- data.table(l = c(TRUE, FALSE, NA), r = as.raw(c(1, 200, 0)))
expect_identical(row_id_by_pattern(D, magnitude = TRUE), row_id_by_pattern(D))

# Threads ---------------------------------------------------------------------

if (hutilscpp:::has_openmp() && requireNamespace("parallel", quietly = TRUE) &&
    parallel::detectCores() >= 2L) {
  D_many <- make_mixed(50000L, 11L)
  D_few <- data.table(x = rep(c(0L, 1L), 25000L))
  set.seed(5)
  D_distinct <- data.table(s = sample(as.character(1:50000)))
  for (D in list(D_many, D_few, D_distinct)) {
    ref <- row_id_by_pattern(D, nThread = 1L)
    expect_identical(row_id_by_pattern(D, nThread = 2L), ref)
    expect_identical(row_id_by_pattern(D, nThread = 2L, magnitude = TRUE),
                     row_id_by_pattern(D, nThread = 1L, magnitude = TRUE))
  }
  expect_identical(row_id_by_pattern(D_many, nThread = 2L), ref_row_id(D_many))
}

# Input is not modified
D <- make_mixed(100L, 2L)
D0 <- copy(D)
invisible(row_id_by_pattern(D))
expect_identical(D, D0)

# Errors ---------------------------------------------------------------------

expect_error(row_id_by_pattern(1:3), "data.frame")
expect_error(row_id_by_pattern(data.table(x = 1L, y = list(1))), "`y`")
expect_error(row_id_by_pattern(data.table(x = 1L, z = 1i)), "`z`")
expect_error(row_id_by_pattern(data.table(x = structure(1, class = "integer64"))), "`x`")
# Matrix-valued and nested data.frame columns are valid data.frame columns
# but do not have one element per row: reject rather than misalign.
DTm <- data.frame(x = I(matrix(c(0, 1, 1, 0), nrow = 2)))
expect_true(nrow(DTm) == 2L)
expect_error(row_id_by_pattern(DTm), "`x`")
DTm2 <- data.frame(a = 1:2)
DTm2$m <- matrix(1:4, nrow = 2)
expect_error(row_id_by_pattern(DTm2), "`m`")
DTn <- data.frame(a = 1:2)
DTn$d <- data.frame(b = 1:2)
expect_error(row_id_by_pattern(DTn), "`d`")
# A one-column matrix has the right length but is still rejected (dim attribute)
DTm1 <- data.frame(a = 1:2)
DTm1$m <- matrix(1:2, nrow = 2)
expect_error(row_id_by_pattern(DTm1), "`m`")
# A column of the wrong length in a hand-built list-based data.frame
DTl <- structure(list(a = 1:3, b = 1:2), class = "data.frame", row.names = 1:3)
expect_error(row_id_by_pattern(DTl), "`b`")
expect_error(row_id_by_pattern(data.table(x = 1L), na_is = 2L), "na_is")
expect_error(row_id_by_pattern(data.table(x = 1L), na_is = NA), "na_is")
expect_error(row_id_by_pattern(data.table(x = 1L), na_is = "0"), "na_is")
expect_error(row_id_by_pattern(data.table(x = 1L), na_is = 0:1), "na_is")
expect_error(row_id_by_pattern(data.table(x = 1L), magnitude = NA), "magnitude")
expect_error(row_id_by_pattern(data.table(x = 1L), max_patterns = 0L), "max_patterns")
expect_error(row_id_by_pattern(data.table(x = 1L), max_patterns = 2.5), "max_patterns")
expect_error(row_id_by_pattern(data.table(x = 1L), max_patterns = NA), "max_patterns")
expect_error(row_id_by_pattern(data.table(x = 1L), nThread = -1L), "nThread")
