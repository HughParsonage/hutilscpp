# Phase 1 invariant grid harness for `and3s`. Issue #42.
#
# Sweeps the (x_type, y_type, op, M_shape, position) dispatch matrix in
# vand2s, comparing `and3s` output to a base-R reference. The reference
# applies NA -> FALSE to logical results to match the documented
# two-valued mask convention (?and3s "Note on NA / NaN").
#
# Cells with `NA` in a unary logical input are deliberately omitted: the
# package's unary init treats NA as TRUE rather than FALSE, which is a
# pre-existing divergence from both base R and the doc'd mask convention.
# That should be reconciled separately.
#
# Layout:
#   Section 1: comparison ops (==, !=, <, <=, >, >=) across numeric type pairs
#   Section 2: between ops (BW/BO/BC) including degenerate ranges
#   Section 3: raw type combinations (the most fragile family per Phase 0)
#   Section 4: %in% / %notin%
#   Section 5: structural invariants (permutation, position, path, type)
#   Section 6: documented NaN -> FALSE convention
#   Section 7: logical and string types

if (requireNamespace("hutilscpp", quietly = TRUE) &&
    requireNamespace("data.table", quietly = TRUE)) {

library(data.table)
library(hutilscpp)

"%(between)%" <- hutilscpp:::`%(between)%`
"%]between[%" <- hutilscpp:::`%]between[%`

n <- 1500  # past the 1000-element fast/fallback boundary in `and3s`

# Coerce NA -> FALSE for the mask-convention reference comparison.
na2f <- function(x) {
  if (is.logical(x) && anyNA(x)) x[is.na(x)] <- FALSE
  x
}

# Compare an `and3s` call to a base-R reference. Wraps suppressMessages
# so cells that legitimately fall back don't spam test output.
g_ <- function(got, want) {
  expect_equal(suppressMessages(got), na2f(want))
}

# x-vectors per type. Values chosen to span typical cases (zeros,
# positives, negatives, edges) so each predicate has both true and
# false rows.
ix <- rep_len(c(-1L, 0L, 1L, 5L, 10L, 100L), n)
dx <- as.double(rep_len(c(-1.5, 0, 1, 5.5, 10, 100), n))
rx <- as.raw(rep_len(c(0L, 1L, 5L, 100L, 200L, 255L), n))
sx <- rep_len(c("a", "b", "c"), n)

# y vectors of length n for M==N branches.
iy <- rep_len(c(0L, 1L, 5L, 10L, 100L, 200L), n)
dy <- as.double(rep_len(c(0, 1.5, 5, 10, 100, 200), n))
ry <- as.raw(rep_len(c(0L, 5L, 10L), n))

# ============================================================================
# Section 1: comparison ops, scalar and vector RHS, numeric type pairs
# ============================================================================

# --- INT x INT ---
g_(and3s(ix != 0L, ix == 5L),  (ix != 0L) & (ix == 5L))
g_(and3s(ix != 0L, ix != 5L),  (ix != 0L) & (ix != 5L))
g_(and3s(ix != 0L, ix <  5L),  (ix != 0L) & (ix <  5L))
g_(and3s(ix != 0L, ix <= 5L),  (ix != 0L) & (ix <= 5L))
g_(and3s(ix != 0L, ix >  5L),  (ix != 0L) & (ix >  5L))
g_(and3s(ix != 0L, ix >= 5L),  (ix != 0L) & (ix >= 5L))
g_(and3s(ix != 0L, ix == iy),  (ix != 0L) & (ix == iy))
g_(and3s(ix != 0L, ix != iy),  (ix != 0L) & (ix != iy))
g_(and3s(ix != 0L, ix <  iy),  (ix != 0L) & (ix <  iy))
g_(and3s(ix != 0L, ix >  iy),  (ix != 0L) & (ix >  iy))

# --- INT x DBL ---
g_(and3s(ix != 0L, ix == 5),    (ix != 0L) & (ix == 5))
g_(and3s(ix != 0L, ix == 5.5),  (ix != 0L) & (ix == 5.5))
g_(and3s(ix != 0L, ix >  5.5),  (ix != 0L) & (ix >  5.5))
g_(and3s(ix != 0L, ix >  -1.5), (ix != 0L) & (ix >  -1.5))
# Non-integer scalar y on order ops -- the #49 fix. The C kernel reduces
# `x op y_frac` to an integer comparison via floor(y_frac). Sweep both
# signs of y to lock down the per-op floor adjustment.
g_(and3s(ix != 0L, ix <  5.5),  (ix != 0L) & (ix <  5.5))
g_(and3s(ix != 0L, ix <  -1.5), (ix != 0L) & (ix <  -1.5))
g_(and3s(ix != 0L, ix <= 5.5),  (ix != 0L) & (ix <= 5.5))
g_(and3s(ix != 0L, ix <= -1.5), (ix != 0L) & (ix <= -1.5))
g_(and3s(ix != 0L, ix >= 5.5),  (ix != 0L) & (ix >= 5.5))
g_(and3s(ix != 0L, ix >= -1.5), (ix != 0L) & (ix >= -1.5))
# Subunit fractions (|y_frac| < 1) -- the corner where trunc-toward-zero
# is 0 and naive sign-of-trunc gating loses the sign of pre_y0.
ix2 <- rep_len(c(-2L, -1L, 0L, 1L, 2L), n)
g_(and3s(ix2 != 999L, ix2 <   0.5), (ix2 != 999L) & (ix2 <   0.5))
g_(and3s(ix2 != 999L, ix2 <  -0.5), (ix2 != 999L) & (ix2 <  -0.5))
g_(and3s(ix2 != 999L, ix2 <=  0.5), (ix2 != 999L) & (ix2 <=  0.5))
g_(and3s(ix2 != 999L, ix2 <= -0.5), (ix2 != 999L) & (ix2 <= -0.5))
g_(and3s(ix2 != 999L, ix2 >   0.5), (ix2 != 999L) & (ix2 >   0.5))
g_(and3s(ix2 != 999L, ix2 >  -0.5), (ix2 != 999L) & (ix2 >  -0.5))
g_(and3s(ix2 != 999L, ix2 >=  0.5), (ix2 != 999L) & (ix2 >=  0.5))
g_(and3s(ix2 != 999L, ix2 >= -0.5), (ix2 != 999L) & (ix2 >= -0.5))
# Out-of-int-range double scalars (these go through a different branch
# in vand2s_ID with INT_MIN/INT_MAX clamping; not affected by #49).
g_(and3s(ix != 0L, ix <  1e10),  (ix != 0L) & (ix <  1e10))
g_(and3s(ix != 0L, ix >  1e10),  (ix != 0L) & (ix >  1e10))
g_(and3s(ix != 0L, ix == 1e10),  (ix != 0L) & (ix == 1e10))
g_(and3s(ix != 0L, ix != 1e10),  (ix != 0L) & (ix != 1e10))

# --- DBL x INT ---
g_(and3s(dx != 0,  dx == 5L),  (dx != 0) & (dx == 5L))
g_(and3s(dx != 0,  dx <  5L),  (dx != 0) & (dx <  5L))
g_(and3s(dx != 0,  dx >  5L),  (dx != 0) & (dx >  5L))
g_(and3s(dx != 0,  dx == iy), (dx != 0) & (dx == iy))

# --- DBL x DBL ---
g_(and3s(dx != 0,  dx == 5.5),  (dx != 0) & (dx == 5.5))
g_(and3s(dx != 0,  dx <  5.5),  (dx != 0) & (dx <  5.5))
g_(and3s(dx != 0,  dx >  5.5),  (dx != 0) & (dx >  5.5))
g_(and3s(dx != 0,  dx == dy),   (dx != 0) & (dx == dy))
g_(and3s(dx != 0,  dx <  dy),   (dx != 0) & (dx <  dy))

# ============================================================================
# Section 2: between ops (BW / BO / BC)
# Sweeps representative (a, b) pairs including degenerate (a == b) and
# inverted (a > b) cases. The y0 == y1 corner is the #47 fix.
# ============================================================================

between_pairs <- list(
  c(-1L, 5L),    # spans zero
  c( 0L, 0L),    # degenerate at zero
  c( 5L, 5L),    # degenerate non-zero (#47)
  c( 1L, 10L),   # positive
  c(-5L,-1L),    # negative
  c(10L, 5L)     # inverted
)
for (ab in between_pairs) {
  a <- ab[1]; b <- ab[2]
  g_(and3s(ix != 999L, ix %between%   c(a, b)),
       (ix != 999L) & (ix %between%   c(a, b)))
  g_(and3s(ix != 999L, ix %(between)% c(a, b)),
       (ix != 999L) & (ix %(between)% c(a, b)))
  g_(and3s(ix != 999L, ix %]between[% c(a, b)),
       (ix != 999L) & (ix %]between[% c(a, b)))
}

# Double bounds, including fractional and out-of-int-range
between_dbl_pairs <- list(c(1.5, 5.5), c(-1.5, 5.5), c(0, 0), c(1e10, 2e10))
for (ab in between_dbl_pairs) {
  a <- ab[1]; b <- ab[2]
  g_(and3s(ix != 999L, ix %between%   c(a, b)),
       (ix != 999L) & (ix %between%   c(a, b)))
  g_(and3s(dx != 999,  dx %between%   c(a, b)),
       (dx != 999)  & (dx %between%   c(a, b)))
  g_(and3s(dx != 999,  dx %(between)% c(a, b)),
       (dx != 999)  & (dx %(between)% c(a, b)))
}

# ============================================================================
# Section 3: raw type combinations (most fragile per Phase 0 review iterations)
# ============================================================================

# --- RAW x RAW scalar ==/!= ---
g_(and3s(rx != as.raw(0), rx == as.raw(5)),
     (rx != as.raw(0)) & (rx == as.raw(5)))
g_(and3s(rx != as.raw(0), rx != as.raw(5)),
     (rx != as.raw(0)) & (rx != as.raw(5)))

# --- RAW x INT scalar (in-range, out-of-range, order ops) ---
g_(and3s(rx != as.raw(0), rx == 5L),
     (rx != as.raw(0)) & (as.integer(rx) == 5L))
g_(and3s(rx != as.raw(0), rx == 256L),
     (rx != as.raw(0)) & FALSE)
g_(and3s(rx != as.raw(0), rx == -1L),
     (rx != as.raw(0)) & FALSE)
# Order ops on raw with in-range integer fall back to base R (kernel
# only handles ==/!=); harness pins they still match base R.
g_(and3s(rx != as.raw(0), rx >  5L),
     (rx != as.raw(0)) & (as.integer(rx) >  5L))
g_(and3s(rx != as.raw(0), rx <= 5L),
     (rx != as.raw(0)) & (as.integer(rx) <= 5L))
# Order ops with out-of-range RHS are constants handled in C.
g_(and3s(rx != as.raw(0), rx >  256L),
     (rx != as.raw(0)) & FALSE)
g_(and3s(rx != as.raw(0), rx >  -1L),
     (rx != as.raw(0)) & TRUE)
g_(and3s(rx != as.raw(0), rx <  -1L),
     (rx != as.raw(0)) & FALSE)
g_(and3s(rx != as.raw(0), rx <= 256L),
     (rx != as.raw(0)) & TRUE)

# --- RAW x DBL scalar (fractional, NaN, out-of-range) ---
g_(and3s(rx != as.raw(0), rx == 5.5),
     (rx != as.raw(0)) & FALSE)
g_(and3s(rx != as.raw(0), rx != 5.5),
     (rx != as.raw(0)) & TRUE)
g_(and3s(rx != as.raw(0), rx >  5.5),
     (rx != as.raw(0)) & (as.integer(rx) >  5.5))
g_(and3s(rx != as.raw(0), rx <= 5.5),
     (rx != as.raw(0)) & (as.integer(rx) <= 5.5))

# --- RAW x RAW M==N (the #37 fall-through path) ---
g_(and3s(rx != as.raw(0), rx == ry),
     (rx != as.raw(0)) & (rx == ry))
g_(and3s(rx != as.raw(0), rx %notin% ry),
     (rx != as.raw(0)) & !(rx %in% ry))

# --- Precomputed raw mask reused as exprA ---
mask_raw <- and3s(rx != as.raw(0), type = "raw")
mask_lgl <- hutilscpp:::raw2lgl(mask_raw)
g_(and3s(mask_raw, rx >  5L),    mask_lgl & (as.integer(rx) >  5L))
g_(and3s(mask_raw, rx >  256L),  mask_lgl & FALSE)
g_(and3s(mask_raw, rx %in% 5L),  mask_lgl & (as.integer(rx) %in% 5L))
g_(and3s(mask_raw, rx == 5.5),   mask_lgl & FALSE)

# ============================================================================
# Section 4: %in% / %notin%
# The wrapper preprocesses %in% / %notin% via finp / fnotinp before
# reaching C, so the dispatch path differs from element-wise comparison.
# ============================================================================

g_(and3s(ix != 0L, ix %in%    c(1L, 5L, 10L)),
     (ix != 0L) & (ix %in%    c(1L, 5L, 10L)))
g_(and3s(ix != 0L, ix %notin% c(1L, 5L, 10L)),
     (ix != 0L) & !(ix %in%   c(1L, 5L, 10L)))
g_(and3s(dx != 0,  dx %in%    c(1, 5.5, 10)),
     (dx != 0)  & (dx %in%    c(1, 5.5, 10)))

# ============================================================================
# Section 5: structural invariants
# ============================================================================

# --- Permutation invariance over a 3-arg AND chain ---
ref3 <- (ix > 1L) & (ix < 100L) & (ix != 0L)
expect_equal(and3s(ix > 1L, ix < 100L, ix != 0L), ref3)
expect_equal(and3s(ix < 100L, ix != 0L, ix > 1L), ref3)
expect_equal(and3s(ix != 0L, ix > 1L, ix < 100L), ref3)

# --- Position invariance (predicate as exprA vs exprB) ---
expect_equal(and3s(ix == 5L, ix != 0L),
             and3s(ix != 0L, ix == 5L))
expect_equal(and3s(ix %between% c(1L, 10L), ix != 0L),
             and3s(ix != 0L, ix %between% c(1L, 10L)))

# --- Path equivalence: small-vector R fallback vs C-side dispatch ---
ix_short <- ix[seq_len(900)]   # n=900 falls through to .et3 R-side
ix_long  <- ix
expect_equal(and3s(ix_short != 0L, ix_short < 5L),
             head(and3s(ix_long != 0L, ix_long < 5L), 900))

# --- Type equivalence: logical / raw / which agree on the same mask ---
out_l <- and3s(ix != 0L, ix == 5L, type = "logical")
out_r <- and3s(ix != 0L, ix == 5L, type = "raw")
out_w <- and3s(ix != 0L, ix == 5L, type = "which")
expect_equal(out_l, hutilscpp:::raw2lgl(out_r))
expect_equal(out_w, which(out_l))

# ============================================================================
# Section 6: documented NaN -> FALSE convention
# ?and3s "Note on NA / NaN" -- NaN comparisons evaluate to FALSE in the
# mask, except != / %notin% against NaN which remain TRUE.
# ============================================================================

g_(and3s(ix != 999L, ix == NaN),  logical(n))           # always FALSE
g_(and3s(ix != 999L, ix != NaN),  ix != 999L)            # always TRUE
g_(and3s(ix != 999L, ix <  NaN),  logical(n))            # always FALSE
g_(and3s(ix != 999L, ix >  NaN),  logical(n))            # always FALSE
g_(and3s(rx != as.raw(0), rx >  NaN),  logical(n))       # always FALSE
g_(and3s(rx != as.raw(0), rx == NaN),  logical(n))       # always FALSE
g_(and3s(rx != as.raw(0), rx != NaN),  rx != as.raw(0))  # always TRUE

# ============================================================================
# Section 7: logical and string types
# (NA inputs avoided; see file header comment.)
# ============================================================================

lx <- rep_len(c(TRUE, FALSE, FALSE, TRUE), n)
ly <- rep_len(c(TRUE, TRUE,  FALSE, TRUE), n)
expect_equal(and3s(lx, ly), lx & ly)
expect_equal(and3s(lx),     lx)

sy <- rep_len(c("a", "c", "b"), n)
g_(and3s(sx == "a", sx != "b"), (sx == "a") & (sx != "b"))
g_(and3s(sx == sy,  sx != "z"), (sx == sy)  & (sx != "z"))

}  # end requireNamespace
