# Phase 1 invariant grid harness for `or3s`. Issue #42.
#
# Sweeps the (x_type, y_type, op, M_shape, position) dispatch matrix
# in vor2s, comparing `or3s` output to a base-R reference. Reference
# applies NA -> FALSE for the mask convention.
#
# Cor3s.c is narrower than Cand3s.c: it implements only the numeric
# type pairs (II / ID / DI / DD), plus LL / L / SS. Raw type pairs
# fall through to the wrapper-side fallback (the wrapper preprocesses
# %in% as logical for `or3s`, so raw `xx1` is uncommon). The harness
# covers what the kernel handles plus a couple of fallback cells for
# safety; broader raw coverage is a Phase 3 concern (#44).

if (requireNamespace("hutilscpp", quietly = TRUE) &&
    requireNamespace("data.table", quietly = TRUE)) {

library(data.table)
library(hutilscpp)

"%(between)%" <- hutilscpp:::`%(between)%`
"%]between[%" <- hutilscpp:::`%]between[%`

n <- 1500

na2f <- function(x) {
  if (is.logical(x) && anyNA(x)) x[is.na(x)] <- FALSE
  x
}
g_ <- function(got, want) {
  expect_equal(suppressMessages(got), na2f(want))
}

ix <- rep_len(c(-1L, 0L, 1L, 5L, 10L, 100L), n)
dx <- as.double(rep_len(c(-1.5, 0, 1, 5.5, 10, 100), n))
sx <- rep_len(c("a", "b", "c"), n)
iy <- rep_len(c(0L, 1L, 5L, 10L, 100L, 200L), n)
dy <- as.double(rep_len(c(0, 1.5, 5, 10, 100, 200), n))

# ============================================================================
# Section 1: comparison ops, scalar and vector RHS, numeric type pairs
# ============================================================================

# --- INT x INT ---
g_(or3s(ix > 99L, ix == 5L),  (ix > 99L) | (ix == 5L))
g_(or3s(ix > 99L, ix != 5L),  (ix > 99L) | (ix != 5L))
g_(or3s(ix > 99L, ix <  5L),  (ix > 99L) | (ix <  5L))
g_(or3s(ix > 99L, ix <= 5L),  (ix > 99L) | (ix <= 5L))
g_(or3s(ix > 99L, ix >  5L),  (ix > 99L) | (ix >  5L))
g_(or3s(ix > 99L, ix >= 5L),  (ix > 99L) | (ix >= 5L))
g_(or3s(ix > 99L, ix == iy),  (ix > 99L) | (ix == iy))
g_(or3s(ix > 99L, ix <  iy),  (ix > 99L) | (ix <  iy))

# --- INT x DBL ---
g_(or3s(ix > 99L, ix == 5),    (ix > 99L) | (ix == 5))
g_(or3s(ix > 99L, ix == 5.5),  (ix > 99L) | (ix == 5.5))
g_(or3s(ix > 99L, ix <  5.5),  (ix > 99L) | (ix <  5.5))
g_(or3s(ix > 99L, ix >  5.5),  (ix > 99L) | (ix >  5.5))
g_(or3s(ix > 99L, ix <  1e10), (ix > 99L) | (ix <  1e10))
g_(or3s(ix > 99L, ix >  1e10), (ix > 99L) | (ix >  1e10))
# Non-integer scalar y on order ops, both signs and the (-1,1) corner
# (the #49 audit). The kernel reduces `x op y_frac` to integer ops via
# floor(y_frac); the |y|<1 cells exercise the sign-loss bug where the
# truncated value is 0.
g_(or3s(ix > 99L, ix <= 5.5),  (ix > 99L) | (ix <= 5.5))
g_(or3s(ix > 99L, ix >= 5.5),  (ix > 99L) | (ix >= 5.5))
g_(or3s(ix > 99L, ix <  -1.5), (ix > 99L) | (ix <  -1.5))
g_(or3s(ix > 99L, ix <= -1.5), (ix > 99L) | (ix <= -1.5))
g_(or3s(ix > 99L, ix >  -1.5), (ix > 99L) | (ix >  -1.5))
g_(or3s(ix > 99L, ix >= -1.5), (ix > 99L) | (ix >= -1.5))
ix2 <- rep_len(c(-2L, -1L, 0L, 1L, 2L), n)
g_(or3s(ix2 > 99L, ix2 <   0.5), (ix2 > 99L) | (ix2 <   0.5))
g_(or3s(ix2 > 99L, ix2 <  -0.5), (ix2 > 99L) | (ix2 <  -0.5))
g_(or3s(ix2 > 99L, ix2 <=  0.5), (ix2 > 99L) | (ix2 <=  0.5))
g_(or3s(ix2 > 99L, ix2 <= -0.5), (ix2 > 99L) | (ix2 <= -0.5))
g_(or3s(ix2 > 99L, ix2 >   0.5), (ix2 > 99L) | (ix2 >   0.5))
g_(or3s(ix2 > 99L, ix2 >  -0.5), (ix2 > 99L) | (ix2 >  -0.5))
g_(or3s(ix2 > 99L, ix2 >=  0.5), (ix2 > 99L) | (ix2 >=  0.5))
g_(or3s(ix2 > 99L, ix2 >= -0.5), (ix2 > 99L) | (ix2 >= -0.5))

# --- DBL x INT ---
g_(or3s(dx > 99,  dx == 5L),  (dx > 99) | (dx == 5L))
g_(or3s(dx > 99,  dx <  5L),  (dx > 99) | (dx <  5L))
g_(or3s(dx > 99,  dx == iy),  (dx > 99) | (dx == iy))

# --- DBL x DBL ---
g_(or3s(dx > 99,  dx == 5.5),  (dx > 99) | (dx == 5.5))
g_(or3s(dx > 99,  dx <  5.5),  (dx > 99) | (dx <  5.5))
g_(or3s(dx > 99,  dx == dy),   (dx > 99) | (dx == dy))

# ============================================================================
# Section 2: between ops, including degenerate ranges
# ============================================================================

between_pairs <- list(
  c(-1L, 5L), c(0L, 0L), c(5L, 5L), c(1L, 10L), c(-5L, -1L), c(10L, 5L)
)
for (ab in between_pairs) {
  a <- ab[1]; b <- ab[2]
  g_(or3s(ix > 99L, ix %between%   c(a, b)),
       (ix > 99L) | (ix %between%   c(a, b)))
  g_(or3s(ix > 99L, ix %(between)% c(a, b)),
       (ix > 99L) | (ix %(between)% c(a, b)))
  g_(or3s(ix > 99L, ix %]between[% c(a, b)),
       (ix > 99L) | (ix %]between[% c(a, b)))
}

between_dbl_pairs <- list(c(1.5, 5.5), c(0, 0), c(1e10, 2e10))
for (ab in between_dbl_pairs) {
  a <- ab[1]; b <- ab[2]
  g_(or3s(dx > 99, dx %between%   c(a, b)),
       (dx > 99) | (dx %between%   c(a, b)))
  g_(or3s(dx > 99, dx %(between)% c(a, b)),
       (dx > 99) | (dx %(between)% c(a, b)))
}

# ============================================================================
# Section 3: %in% / %notin%
# ============================================================================

g_(or3s(ix > 99L, ix %in%    c(1L, 5L, 10L)),
     (ix > 99L) | (ix %in%    c(1L, 5L, 10L)))
g_(or3s(ix > 99L, ix %notin% c(1L, 5L, 10L)),
     (ix > 99L) | !(ix %in%   c(1L, 5L, 10L)))

# Phase 3 raw paths: or3s now reaches the RR/RI/RD kernels directly
# (pre-Phase-3 it fell through to OR3__UNSUPPORTED_TYPEX). A few
# regression checks for the edges that bit the kernel:
#   1. mismatched-length raw vector compare must not over-read y;
#   2. raw vs logical/character must not silently dispatch to a
#      unary raw kernel that ignores y.
rx <- as.raw(rep_len(c(0L, 1L, 5L, 100L, 200L, 255L), n))
g_(or3s(rx == as.raw(5)),           rx == as.raw(5))
g_(or3s(rx %in% as.raw(c(5, 100))), rx %in% as.raw(c(5, 100)))
g_(or3s(rx == as.raw(c(1, 2))),     rx == as.raw(c(1, 2)))   # M != N
g_(or3s(rx != as.raw(c(1, 2))),     rx != as.raw(c(1, 2)))
g_(or3s(rx == c(1L, 2L, 5L)),       rx == c(1L, 2L, 5L))     # short int y
g_(or3s(rx == c(1, 2.5)),           rx == c(1, 2.5))         # short dbl y
g_(or3s(rx == FALSE),               rx == FALSE)             # logical y
g_(or3s(rx != FALSE),               rx != FALSE)

# Precomputed raw mask reused as exprB (NIL yy2; unary raw-mask path).
# Without the unary kernel route the wrapper falls back and re-evaluates
# the first predicate -- bad for non-deterministic exprA.
mask_raw_or <- or3s(rx > as.raw(100), type = "raw")
mask_lgl_or <- hutilscpp:::raw2lgl(mask_raw_or)
g_(or3s(rx == as.raw(5),  mask_raw_or),
     (rx == as.raw(5)) | mask_lgl_or)
g_(or3s(rx == as.raw(5), !mask_raw_or),
     (rx == as.raw(5)) | (mask_raw_or == as.raw(0)))

# Externally-supplied raw mask with non-{0,1} truthy bytes: entry-side
# and dispatcher-side NIL-y handlers must agree on `!m` semantics
# (byte == 0, not byte != 1). See test-and3s-grid for the AND analogue.
m_ext_or <- as.raw(rep_len(c(0L, 1L, 2L, 3L), n))
all_false <- rep_len(FALSE, n)        # bare-symbol exprA so the wrapper takes the C path
g_(or3s(m_ext_or),                  as.integer(m_ext_or) != 0)
g_(or3s(!m_ext_or),                 as.integer(m_ext_or) == 0)
expect_equal(or3s(!m_ext_or), or3s(all_false, !m_ext_or))

# ============================================================================
# Section 4: structural invariants
# ============================================================================

# --- Permutation invariance ---
ref3 <- (ix < 0L) | (ix == 5L) | (ix > 99L)
expect_equal(or3s(ix < 0L, ix == 5L, ix > 99L), ref3)
expect_equal(or3s(ix > 99L, ix < 0L, ix == 5L), ref3)
expect_equal(or3s(ix == 5L, ix > 99L, ix < 0L), ref3)

# --- Position invariance ---
expect_equal(or3s(ix == 5L, ix > 99L),
             or3s(ix > 99L, ix == 5L))

# --- Path equivalence ---
ix_short <- ix[seq_len(900)]
expect_equal(or3s(ix_short < 0L, ix_short == 5L),
             head(or3s(ix < 0L, ix == 5L), 900))

# --- Type equivalence ---
out_l <- or3s(ix > 99L, ix == 5L, type = "logical")
out_r <- or3s(ix > 99L, ix == 5L, type = "raw")
out_w <- or3s(ix > 99L, ix == 5L, type = "which")
expect_equal(out_l, hutilscpp:::raw2lgl(out_r))
expect_equal(out_w, which(out_l))

# ============================================================================
# Section 5: documented NaN -> FALSE convention
# (Symmetric to and3s. NaN comparisons -> FALSE; != / %notin% -> TRUE.)
# ============================================================================

g_(or3s(ix > 99L, ix == NaN),  ix > 99L)             # NaN -> FALSE; OR identity
g_(or3s(ix > 99L, ix != NaN),  rep_len(TRUE, n))     # NaN != -> always TRUE
g_(or3s(ix > 99L, ix <  NaN),  ix > 99L)
g_(or3s(ix > 99L, ix >  NaN),  ix > 99L)

# ============================================================================
# Section 6: logical and string types
# ============================================================================

lx <- rep_len(c(TRUE, FALSE, FALSE, TRUE), n)
ly <- rep_len(c(FALSE, FALSE, FALSE, TRUE), n)
expect_equal(or3s(lx, ly), lx | ly)

sy <- rep_len(c("a", "c", "b"), n)
g_(or3s(sx == "a", sx != "b"), (sx == "a") | (sx != "b"))
g_(or3s(sx == sy,  sx == "z"), (sx == sy)  | (sx == "z"))

}  # end requireNamespace
