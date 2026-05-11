# Phase 4 (#45) -- explicit `na` / `unsupported` / `recycle` arguments
# at the and3s/or3s R boundary. Defaults preserve CRAN behaviour;
# non-defaults are user opt-ins.

if (requireNamespace("hutilscpp", quietly = TRUE) &&
    requireNamespace("data.table", quietly = TRUE)) {

library(data.table)
library(hutilscpp)

n <- 1500
ix     <- rep_len(c(0L, 1L, 2L, 3L, 5L), n)
ix_na  <- rep_len(c(0L, 1L, NA_integer_, 5L), n)
dx     <- as.numeric(ix)

# ============================================================================
# Defaults must match CRAN behaviour exactly (`na = "C"`).
# ============================================================================
expect_equal(and3s(ix > 0L), and3s(ix > 0L, na = "C", unsupported = "fallback", recycle = "base"))
expect_equal(or3s(ix > 99L), or3s(ix > 99L, na = "C", unsupported = "fallback", recycle = "base"))

# ============================================================================
# unsupported = "error"
# ============================================================================
# Length-2 non-between RHS: kernel sets err -> wrapper falls back by default,
# stops with a clear message under unsupported = "error".
expect_silent(suppressMessages(and3s(ix == c(2L, -1L), unsupported = "fallback")))
expect_error(and3s(ix == c(2L, -1L), unsupported = "error"),
             "unsupported type/op/length")
expect_error(or3s(ix == c(2L, -1L), unsupported = "error"),
             "unsupported type/op/length")
# Forwarded through sum_*3s
expect_error(sum_and3s(ix == c(2L, -1L), unsupported = "error"),
             "unsupported type/op/length")
expect_error(sum_or3s(ix == c(2L, -1L), unsupported = "error"),
             "unsupported type/op/length")

# Supported expressions are unaffected by `unsupported = "error"`.
expect_equal(and3s(ix == 5L, unsupported = "error"), ix == 5L)
expect_equal(or3s(ix == 5L,  unsupported = "error"), ix == 5L)

# ============================================================================
# recycle = "strict"
# ============================================================================
# Default ("base"): wrapper falls back so base R's recycling rule applies.
expect_equal(suppressMessages(and3s(ix == c(2L, -1L))),  ix == c(2L, -1L))
expect_equal(suppressMessages(or3s(ix == c(2L, -1L))),   ix == c(2L, -1L))

# strict: error for any RHS length not in {1, N, 2 if between}
expect_error(and3s(ix == c(2L, -1L), recycle = "strict"),
             "recycle")
expect_error(or3s(ix == c(2L, -1L),  recycle = "strict"),
             "recycle")
expect_error(sum_and3s(ix == c(2L, -1L), recycle = "strict"),
             "recycle")
expect_error(sum_or3s(ix == c(2L, -1L), recycle = "strict"),
             "recycle")

# Allowed shapes pass under strict: M==1, M==N, between with M==2
expect_equal(and3s(ix == 5L,        recycle = "strict"), ix == 5L)
expect_equal(and3s(ix == ix,        recycle = "strict"), rep_len(TRUE, n))
expect_equal(and3s(ix %between% c(1L, 5L), recycle = "strict"),
             ix %between% c(1L, 5L))

# Between with M != 2 still errors under strict (length 3 isn't allowed)
expect_error(and3s(ix %between% c(1L, 2L, 3L), recycle = "strict"),
             "recycle")

# ============================================================================
# na = "base"
# ============================================================================
# Default ("C"): historical C-level semantics. For this integer
# comparison, NA_INTEGER is interpreted as INT_MIN and the predicate is
# false at missing positions.
got_def <- and3s(ix_na > 0L)
expect_false(anyNA(got_def))
expect_equal(sum(got_def), sum(ix_na > 0L, na.rm = TRUE))

# "base": full base R semantics, NA propagates.
got_base    <- and3s(ix_na > 0L, na = "base")
got_base_or <- or3s(ix_na > 0L,  na = "base")
expect_equal(got_base,    ix_na > 0L)
expect_equal(got_base_or, ix_na > 0L)

# Two-predicate AND/OR chain with NA in either side preserves base semantics.
ix_a <- ix_na
ix_b <- rep_len(c(1L, NA_integer_, 5L, 0L), n)
expect_equal(and3s(ix_a > 0L, ix_b == 5L, na = "base"),
             (ix_a > 0L) & (ix_b == 5L))
expect_equal(or3s(ix_a > 0L,  ix_b == 5L, na = "base"),
             (ix_a > 0L) | (ix_b == 5L))

# Three-predicate chain with NA only in the THIRD predicate. Pre-fix
# the recursive call requested type = "raw", which dropped NA via
# lgl2raw before .and_raw / .or_raw combined the masks. The recursive
# call now requests type = "logical" and combines in R, preserving NA.
nx <- rep_len(c(1L, 2L), n)
ny <- rep_len(c(1L, 2L), n)
nz <- rep_len(c(1L, NA_integer_), n)
expect_equal(and3s(nx > 0L, ny > 0L, nz > 0L, na = "base"),
             (nx > 0L) & (ny > 0L) & (nz > 0L))
expect_equal(or3s(nx > 99L, ny > 99L, nz > 0L, na = "base"),
             (nx > 99L) | (ny > 99L) | (nz > 0L))
expect_equal(sum_and3s(nx > 0L, ny > 0L, nz > 0L, na = "base"),
             sum((nx > 0L) & (ny > 0L) & (nz > 0L)))
expect_equal(sum_or3s(nx > 99L, ny > 99L, nz > 0L, na = "base"),
             sum((nx > 99L) | (ny > 99L) | (nz > 0L)))

# Supported expressions without NA take the kernel path even under na="base".
expect_equal(and3s(ix > 0L,  na = "base"),    ix > 0L)
expect_equal(or3s(ix == 5L,  na = "base"),    ix == 5L)

# ============================================================================
# Combined sanity: the three options compose
# ============================================================================
expect_error(and3s(ix == c(2L, -1L), unsupported = "error", recycle = "strict"),
             "recycle")  # strict fires first
expect_equal(and3s(ix > 0L, na = "base", recycle = "strict", unsupported = "error"),
             ix > 0L)    # all three options + supported expr -> kernel path

# `recycle = "strict"` must fire even when `na = "base"` would
# otherwise route NA-containing predicates to the base R fallback.
# Pre-fix, the NA-base branch returned via Reduce() before the strict
# check ran, so invalid lengths bypassed strict mode.
expect_error(and3s(ix_na == c(2L, -1L), na = "base", recycle = "strict"),
             "recycle")
expect_error(or3s(ix_na  == c(2L, -1L), na = "base", recycle = "strict"),
             "recycle")

# The same NA-base fallback must not bypass explicit error modes for
# unsupported first/second predicates, or for exprC / further predicates.
expect_error(and3s(ix == c(2L, NA_integer_), na = "base", unsupported = "error"),
             "unsupported type/op/length")
expect_error(or3s(ix == c(2L, NA_integer_), na = "base", unsupported = "error"),
             "unsupported type/op/length")
expect_error(and3s(ix_na > 0L, ix > 0L, ix == c(2L, -1L),
                   na = "base", recycle = "strict"),
             "recycle")
expect_error(or3s(ix_na > 0L, ix > 0L, ix == c(2L, -1L),
                  na = "base", recycle = "strict"),
             "recycle")
expect_error(and3s(ix_na > 0L, ix > 0L, ix == c(2L, -1L),
                   na = "base", unsupported = "error"),
             "unsupported type/op/length")
expect_error(or3s(ix_na > 0L, ix > 0L, ix == c(2L, -1L),
                  na = "base", unsupported = "error"),
             "unsupported type/op/length")

# ============================================================================
# Small-vector shortcut respects the new options
# ============================================================================
# Inputs of length <= 1000 go through .et3() / .or3() (small-vector
# fast path). Pre-fix this path bypassed na/recycle/unsupported, so
# behaviour was input-size-dependent.
short_na <- c(TRUE, NA, FALSE, NA)
# Default na = "C" preserves the historical small-vector shortcut.
expect_equal(and3s(short_na), short_na)
expect_equal(or3s(short_na),  short_na)
# na = "false" must coerce NA -> FALSE.
expect_equal(and3s(short_na, na = "false"), c(TRUE, FALSE, FALSE, FALSE))
expect_equal(or3s(short_na,  na = "false"), c(TRUE, FALSE, FALSE, FALSE))
# na = "base" preserves NA on short inputs too.
expect_equal(and3s(short_na, na = "base"), short_na)
expect_equal(or3s(short_na,  na = "base"), short_na)
# recycle = "strict" must error even on short inputs.
expect_error(and3s(1:10 == c(1L, 2L), recycle = "strict"), "recycle")
expect_error(or3s(1:10  == c(1L, 2L), recycle = "strict"), "recycle")
# unsupported = "error" must also bypass the small-vector shortcut --
# pre-fix the shortcut only checked na and recycle, so the kernel-side
# error path was silently skipped for short inputs.
expect_error(and3s(1:10 == c(1L, 2L), unsupported = "error"),
             "unsupported type/op/length")
expect_error(or3s(1:10  == c(1L, 2L), unsupported = "error"),
             "unsupported type/op/length")

# ============================================================================
# na = "false" audits: missing predicate results become FALSE, even
# where the C-level default treats the missing value as TRUE/truthy.
long_na <- rep_len(c(TRUE, NA, FALSE, NA), n)
expect_equal(and3s(long_na), rep_len(c(TRUE, TRUE, FALSE, TRUE), n))
expect_equal(or3s(long_na),  rep_len(c(TRUE, TRUE, FALSE, TRUE), n))
expect_equal(and3s(long_na, na = "false"), rep_len(c(TRUE, FALSE, FALSE, FALSE), n))
expect_equal(or3s(long_na,  na = "false"), rep_len(c(TRUE, FALSE, FALSE, FALSE), n))
expect_equal(and3s(ix != NA_integer_, na = "false"), rep_len(FALSE, n))
expect_equal(or3s(ix != NA_integer_,  na = "false"), rep_len(FALSE, n))
rx <- as.raw(rep_len(c(0L, 1L, 5L, 255L), n))
expect_equal(and3s(rx != NaN, na = "false"), rep_len(FALSE, n))
expect_equal(or3s(rx != NaN,  na = "false"), rep_len(FALSE, n))
expect_error(and3s(ix_na > 0L, ix > 0L, ix == c(2L, -1L),
                   na = "false", recycle = "strict"),
             "recycle")
expect_error(or3s(ix_na > 0L, ix > 0L, ix == c(2L, -1L),
                  na = "false", recycle = "strict"),
             "recycle")
expect_error(and3s(ix_na > 0L, ix > 0L, ix == c(2L, -1L),
                   na = "false", unsupported = "error"),
             "unsupported type/op/length")
expect_error(or3s(ix_na > 0L, ix > 0L, ix == c(2L, -1L),
                  na = "false", unsupported = "error"),
             "unsupported type/op/length")

# ============================================================================
# sum_*3s preserves NA under na = "base"
# ============================================================================
# Default (na = "C") uses the C-level raw mask; for this integer
# comparison it matches `sum(., na.rm = TRUE)`.
expect_equal(sum_and3s(ix_na > 0L),                 sum(ix_na > 0L, na.rm = TRUE))
expect_equal(sum_or3s( ix_na > 0L),                 sum(ix_na > 0L, na.rm = TRUE))
# na = "false" explicitly drops missing predicate results.
expect_equal(sum_and3s(long_na, na = "false"),      sum(long_na, na.rm = TRUE))
expect_equal(sum_or3s( long_na, na = "false"),      sum(long_na, na.rm = TRUE))
# na = "base" must propagate NA into the sum -- mirrors `sum(. & .)`.
expect_equal(sum_and3s(ix_na > 0L, na = "base"),    sum(ix_na > 0L))
expect_equal(sum_or3s( ix_na > 0L, na = "base"),    sum(ix_na > 0L))
# Two-predicate AND / OR also propagates.
expect_equal(sum_and3s(ix_na > 0L, ix_b == 5L, na = "base"),
             sum((ix_na > 0L) & (ix_b == 5L)))
expect_equal(sum_or3s( ix_na > 0L, ix_b == 5L, na = "base"),
             sum((ix_na > 0L) | (ix_b == 5L)))

}  # end requireNamespace
