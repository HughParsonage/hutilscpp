library(data.table)
library(hutilscpp)
do_and3 <- hutilscpp:::do_and3
"%(between)%" <- hutilscpp:::`%(between)%`
"%]between[%" <- hutilscpp:::`%]between[%`
# })
# test_that("and3s works", {
x <- rep_len(c(5L, -5:10), 1e5)
expect_false(any(and3s(x %between% c(2e10, 3e10))))
expect_true(all(and3s(x %between% c(-2e10, 3e10))))
expect_equal(and3s(x %between% c(4.5, 5)), x %between% c(4.5, 5))
expect_false(any(and3s(x %between% c(-2e10, -1e10))))
expect_false(any(and3s(x %(between)% c(2e10, 3e10))))
expect_true(all(and3s(x %]between[% c(2e10, 3e10))))

expect_equal(and3s(x %(between)% c(1.2, 1e10)),
             x %(between)% c(1.2, 1e10))
expect_equal(and3s(x %]between[% c(1.2, 1e10)),
             x %]between[% c(1.2, 1e10))

x <- 1:10000
expect_true(all(and3s(x != 0L,
                      x >= 1L,
                      x < 10001L)))
expect_true(all(and3s(x > 0L,
                      x != 0L,
                      x <= 10001L)))
expect_true(all(and3s(x > 0L,
                      x <= 10001L,
                      x != 0L)))

y <- integer(5e3)
expect_equal(and3s(y > -1L, y >= 0L, y <= 0L),
             y > -1L & y >= 0L & y <= 0L)
expect_equal(and3s(y != -1L, y >= 0L, y <= 0L),
             y != -1L & y >= 0L & y <= 0L)
expect_equal(and3s(y != -1L, y == 0L, y < 1L),
             y != -1L & y >= 0L & y < 1L)
expect_equal(and3s(y != -1L, y == 0L, y < -1L),
             y != -1L & y >= 0L & y < -1L)

# })

# test_that("and3s works for %in%", {

i <- rep_len(1:15, 1e7)
j <- rep_len(1:16, 1e7)
k <- rep_len(1:17, 1e7)

expect_equal(and3s(i %in% c(5:8), j > 3L, k %in% c(1L, 3L, 4L)),
             and3(i %in% c(5:8), j > 3L, k %in% c(1L, 3L, 4L)))
# %in% 1:9 -> %between% c(1, 9)
expect_equal(and3s(i %in% c(5:8), j > 3L, k %in% c(1:9)),
             and3(i %in% c(5:8), j > 3L, k %in% c(1:9)))
expect_equal(and3s(i %in% c(5:8), k %in% c(1:9), j > 3L),
             and3(i %in% c(5:8), j > 3L, k %in% c(1:9)))


expect_equal(and3s(i %in% c(5:8), j > 3L, k == 9L),
             and3(i %in% c(5:8), j > 3L, k == 9L))
expect_equal(and3s(j > 3L, k == 9L, i %in% c(5:8)),
             and3(i %in% c(5:8), j > 3L, k == 9L))
expect_equal(and3s(i %between% c(9L, 11L), j != 4L, k >= 9L),
             and3(i %between% c(9L, 11L), j != 4L, k >= 9L))
expect_equal(and3s(j != 4L, i %between% c(9L, 11L), k >= 9L),
             and3(i %between% c(9L, 11L), j != 4L, k >= 9L))
expect_equal(and3s(j != 4L, k >= 9L, i %between% c(9L, 11L)),
             and3(i %between% c(9L, 11L), j != 4L, k >= 9L))
expect_equal(and3s(i %in% c(9L, 11L), j != 4L, k >= 9L),
             and3(i %in% c(9L, 11L), j != 4L, k >= 9L))

abc <- 1:5e3
expect_equal(and3s(abc %in% c(2L, 1L), abc != 3L, abc != 1L),
             and3(abc %in% c(2L, 1L), abc != 3L, abc != 1L))
expect_equal(and3s(abc %in% c(2L, 1L, 5L), abc != 3L),
             and3(abc %in% c(2L, 1L, 5L), abc != 3L))
C3 <- abc %in% c(2, 1, 5)
expect_equal(and3s(abc %in% c(2L, 1L, 5L), abc != 3L, C3),
             and3(abc %in% c(2L, 1L, 5L), abc != 3L, C3))
# })

# test_that("and3s length 3 %in%", {
x <- 1:10e3
expect_equal(and3s(x %in% c(3, 1, 2), x %in% c(2, 3, 1)),
             `&`(x %in% c(3, 1, 2), x %in% c(2, 3, 1)))
# })







# test_that("and3 mixture of symbols and calls", {

x <- c(1:10, 9L)
y <- c(0:10)
z <- c(2:10, 9L, -1L)
A <- x > 1L
B <- y > 2L
C <- z > 3L


expect_equal(and3s(x > 1L,
                   y > 2L,
                   z > 3L),
             and3(x > 1L,
                  y > 2L,
                  z > 3L))
expect_equal(and3s(x > 1L,
                   y > 2L),
             and3(x > 1L,
                  y > 2L))
expect_equal(and3s(A,
                   y > 2L,
                   z > 3L),
             and3(A,
                  y > 2L,
                  z > 3L))
expect_equal(and3s(y > 2L,
                   A,
                   z > 3L),
             and3(A,
                  y > 2L,
                  z > 3L))
expect_equal(and3s(y > 2L,
                   z > 3L,
                   A),
             and3(A,
                  y > 2L,
                  z > 3L))
expect_equal(and3s(A,
                   B,
                   C),
             and3(A,
                  B,
                  z > 3L))
expect_equal(and3s(A,
                   B,
                   z > 3L),
             and3(A,
                  B,
                  C))
expect_equal(and3s(x %in% c(4L, 2L),
                   B,
                   C),
             and3(x %in% c(4L, 2L),
                  B,
                  C))
XX <- rep(x, 50)
for (i in XX) {
  expect_equal(and3s(x %in% c(4L, 2L, 8L),
                     B,
                     C),
               and3(x %in% c(4L, 2L, 8L),
                    B,
                    C))
}


# })

if (at_home() && hutilscpp:::is64bit()) {
  tryCatch({
    x <- rep_len(c(1:100, 2L), 1e8)
    B <- rep_len(sample(c(TRUE, FALSE), size = 55, replace = TRUE), 1e8)
    C <- rep_len(sample(c(TRUE, FALSE), size = 55, replace = TRUE), 1e8)
  },
  error = function(e) {
    message(e$m)
  })
  if (exists("B")) {
  expect_equal(and3s(x %in% c(4L, 2L),
                     B,
                     C),
               and3(x %in% c(4L, 2L),
                    B,
                    C))
  expect_equal(and3s(x %in% c(4L, 2L, 9L, 10L, 11L),
                     B,
                     C),
               and3(x %in% c(4L, 2L, 9L, 10L, 11L),
                    B,
                    C))
  expect_equal(and3s(exprB = x %in% c(4L, 2L, 9L, 10L, 11L),
                     B,
                     C),
               and3(x %in% c(4L, 2L, 9L, 10L, 11L),
                    B,
                    C))
  expect_equal(and3s(exprC = x %in% c(4L, 2L, 9L, 10L, 11L),
                     B,
                     C),
               and3(x %in% c(4L, 2L, 9L, 10L, 11L),
                    B,
                    C))
  }
}


# test_that("do_par_in", {
do_par_in <- hutilscpp:::do_par_in
for (i in 1:10) {
  x <- rep_len(c(0L, 11:4), 1e7)
  y <- c(5:3, 13:18)
  expect_equal(x %in% y,
               do_par_in(x, y))
}
# })

if (at_home()) {
  # skip_on_cran()
  # skip_on_travis()
  # skip_on_appveyor()
  x <-
    tryCatch(integer(1e9),
             error = function(e) {
               integer(1e7)
             })
  expect_false(any(and3s(x == 1L, x == 0L)))
}

# test_that("and3s ... and parent frames", {

# Create pathologically confusing names in parent environment
# This should pick up if we have not eval.parent'd correctly
x <- integer(0)
y <- integer(0)

exprA <- c(1:10)
exprB <- 1:10 + 5L
exprC <- 1:10 + 1L

and <- `&`
and3 <- function(i, j, k) and(and(i, j), k)

A <- 1L
sexprA <- 1L
sexprB <- 10L

expect_equal(and3s(exprA >= A, exprB >= sexprA, exprC >= 1L,
                   exprA <= 6L, exprB <= 7L, exprC <= sexprB),
             and(and3(exprA >= 1L, exprB >= 1L, exprC >= 1L),
                 and3(exprA <= 6L, exprB <= 7L, exprC <= 10L)))

expect_equal(and3s(exprA >= A, exprB >= sexprA, exprC >= 1L,
                   exprA <= 6L, exprB <= 7L, exprC <= sexprB,
                   exprA <= 6L, exprB <= 7L, exprC <= sexprB),
             and(and3(exprA >= 1L, exprB >= 1L, exprC >= 1L),
                 and3(exprA <= 6L, exprB <= 7L, exprC <= 10L)))

some_function_with_an_env <- function() {
  exprB <- 1:10 + 4L
  all_eq <- function(a, b) isTRUE(all.equal(a, b))

  `&&`(all_eq(and3s(exprA >= A, exprB >= sexprA, exprC >= 1L,
                    exprA <= 6L, exprB <= 7L, exprC <= sexprB),
              and(and3(exprA >= 1L, exprB >= 1L, exprC >= 1L),
                  and3(exprA <= 6L, exprB <= 7L, exprC <= 10L))),

       all_eq(and3s(exprA >= A, exprB >= sexprA, exprC >= 1L,
                    exprA <= 6L, exprB <= 7L, exprC <= sexprB,
                    exprA <= 6L, exprB <= 7L, exprC <= sexprB),
              and(and3(exprA >= 1L, exprB >= 1L, exprC >= 1L),
                  and3(exprA <= 6L, exprB <= 7L, exprC <= 10L))))
}
expect_true(some_function_with_an_env())

# })

# test_that("logical symbol", {
A <- logical(2)
B <- logical(2)
C <- c(TRUE, TRUE)
expect_equal(and3s(A, B, C), c(FALSE, FALSE))
expect_equal(or3s(A, B, C), c(TRUE, TRUE))
# })

# test_that("%in%", {
xx <- 1:10
bb <- 5:200

xx <- rep_len(xx, length(bb))
expect_equal(and3s(xx %in% bb, xx == 2, bb >= 5), ((xx %in% bb) & (xx == 2 & bb >= 5)))
expect_equal(and3s(xx == 2, xx %in% bb, bb >= 5), ((xx %in% bb) & (xx == 2 & bb >= 5)))
expect_equal(and3s(xx == 2, bb >= 5, xx %in% bb), ((xx %in% bb) & (xx == 2 & bb >= 5)))
expect_equal( or3s(xx %in% bb, xx == 2, bb >= 5), ((xx %in% bb) | (xx == 2 | bb >= 5)))
expect_equal( or3s(xx == 2, xx %in% bb, bb >= 5), ((xx %in% bb) | (xx == 2 | bb >= 5)))
expect_equal( or3s(xx == 2, bb >= 5, xx %in% bb), ((xx %in% bb) | (xx == 2 | bb >= 5)))
# })

# Regression: AND-chain accumulator must not be overwritten by the
# `uc_betweenidd` y0i == y1i fast path. Constructed so the first term
# is FALSE exactly where the between term is TRUE — an overwrite would
# flip those positions to TRUE.
nb <- 5e3
ib <- rep_len(1:10, nb)
expect_equal(and3s(ib != 5L, ib %between% c(5, 5)),
             (ib != 5L) & (ib >= 5L & ib <= 5L))
expect_equal(and3s(ib != 5L, ib %between% c(4.5, 5.4)),
             (ib != 5L) & (ib >= 5L & ib <= 5L))
expect_equal(and3s(ib != 5L, ib %between% c(5, 5), ib < 11L),
             (ib != 5L) & (ib >= 5L & ib <= 5L) & (ib < 11L))

# Regression: AND-chain accumulator must not be overwritten by the
# `vand2s_DI` M == 2 fast path (double x, integer bounds).
xdb <- as.double(rep_len(1:10, nb))
expect_equal(and3s(xdb != 5, xdb %between% c(3L, 7L)),
             (xdb != 5) & (xdb >= 3 & xdb <= 7))
expect_equal(and3s(xdb != 5, xdb %(between)% c(2L, 8L)),
             (xdb != 5) & (xdb > 2 & xdb < 8))
expect_equal(and3s(xdb == 5, xdb %]between[% c(3L, 7L)),
             (xdb == 5) & (xdb <= 3 | xdb >= 7))

# Regression for #47: vand2s_II M==2 OP_BO/OP_BC with y0==y1.
# The y0==y1 branch was only handled for OP_BW; OP_BO and OP_BC fell
# through to the general y0<y1 code which gave wrong answers due to
# unsigned wrap-around in `betweeniiuu(x, y0+1, y0-1)`.
expect_equal(and3s(ib != 0L, ib %(between)% c(5L, 5L)),
             (ib != 0L) & (ib > 5L & ib < 5L))
expect_equal(and3s(ib != 0L, ib %]between[% c(5L, 5L)),
             (ib != 0L) & (ib <= 5L | ib >= 5L))
# y0==y1 in first position too (not just second)
expect_equal(and3s(ib %]between[% c(5L, 5L), ib != 0L),
             (ib %]between[% c(5L, 5L)) & (ib != 0L))

# Regression for #40: when the C dispatcher returns NULL and we fall back
# to base `&`, the user's `type` argument must be honoured. An inner
# `return()` was previously short-circuiting before the type conversion.
# Trigger via INTSXP x with RAWSXP y, which lands on the unsupported-y
# branch in vand2s and forces a NULL return from `Cands`.
ix_fb <- 1:nb
ry_fb <- as.raw(rep_len(c(0L, 1L), nb))
out_l <- suppressMessages(and3s(ix_fb == ry_fb, type = "logical"))
expect_true(is.logical(out_l))
out_r <- suppressMessages(and3s(ix_fb == ry_fb, type = "raw"))
expect_true(is.raw(out_r))
out_w <- suppressMessages(and3s(ix_fb == ry_fb, type = "which"))
expect_true(is.integer(out_w))
expect_equal(out_w, which(ix_fb == ry_fb))

# Regression for #38: vand2s_RI/RD M==1 must not clobber the AND-chain when
# the scalar is out of raw range. OP_NI/OP_NE with out-of-range scalar is
# always-true → no-op; previously memset to 1 wiped earlier predicates.
rr_chain <- as.raw(rep_len(c(0L, 5L), nb))
expect_equal(and3s(rr_chain == as.raw(5), rr_chain %notin% c(-1L)),
             (rr_chain == as.raw(5)) & (rep_len(TRUE, nb)))
expect_equal(and3s(rr_chain == as.raw(5), rr_chain %notin% c(-1)),
             (rr_chain == as.raw(5)) & (rep_len(TRUE, nb)))
expect_equal(and3s(rr_chain == as.raw(5), rr_chain != 256L),
             (rr_chain == as.raw(5)) & (rep_len(TRUE, nb)))
# Always-false direction (OP_IN with out-of-range) should AND with 0.
expect_false(any(and3s(rr_chain == as.raw(5), rr_chain %in% c(-1L))))

# Regression for #38 (review follow-up): out-of-range raw order comparisons
# must not silently no-op. The original out-of-range short-circuit covered
# IN/EQ/NI/NE only; for order ops the kernel must signal err so the R
# wrapper falls back to base `&`, matching base R semantics.
expect_equal(suppressMessages(and3s(rr_chain != as.raw(0), rr_chain > 256L)),
             (rr_chain != as.raw(0)) & (as.integer(rr_chain) > 256L))
expect_equal(suppressMessages(and3s(rr_chain != as.raw(0), rr_chain > -1L)),
             (rr_chain != as.raw(0)) & (as.integer(rr_chain) > -1L))
expect_equal(suppressMessages(and3s(rr_chain != as.raw(0), rr_chain < -1L)),
             (rr_chain != as.raw(0)) & (as.integer(rr_chain) < -1L))
expect_equal(suppressMessages(and3s(rr_chain != as.raw(0), rr_chain <= 256L)),
             (rr_chain != as.raw(0)) & (as.integer(rr_chain) <= 256L))

# Regression for #39: vand2s_RD M==1 must not truncate non-integer scalars.
# `raw_x %in% c(5.5)` should be all FALSE; `%notin% c(5.5)` should be all TRUE.
expect_false(any(and3s(rr_chain == as.raw(5), rr_chain %in% c(5.5))))
expect_equal(and3s(rr_chain == as.raw(5), rr_chain %notin% c(5.5)),
             rr_chain == as.raw(5))
expect_false(any(and3s(rr_chain != as.raw(0), rr_chain == 5.5)))
# Order ops with non-integer RHS must fall back, not no-op.
expect_equal(suppressMessages(and3s(rr_chain != as.raw(0), rr_chain > 5.5)),
             (rr_chain != as.raw(0)) & (as.integer(rr_chain) > 5.5))
expect_equal(suppressMessages(and3s(rr_chain != as.raw(0), rr_chain <= 5.5)),
             (rr_chain != as.raw(0)) & (as.integer(rr_chain) <= 5.5))

# Regression for #37: missing `break;` after OP_NI in vand2s_R{R,I,D} M==N
# branch — silently fell through to OP_NE. Currently latent (compatible
# semantics), but pin the contract so the next refactor doesn't trip.
rr_ni  <- as.raw(rep_len(c(0L, 5L, 10L), nb))
rr_tbl <- as.raw(c(5L, 10L))                    # M=2 != N
i_tbl  <- as.integer(rr_tbl)
d_tbl  <- as.numeric(rr_tbl)
# The wrapper rewrites `%notin% short_table` via fnotinp before reaching the
# C dispatcher, so to exercise the M==N raw fall-through we need a vector
# call that lands directly on vand2s_R*. Equality with M==N suffices to
# pin the dispatch invariant.
rr_eq  <- as.raw(rep_len(c(0L, 5L), nb))
expect_equal(and3s(rr_eq != as.raw(0), rr_eq == rr_eq),
             (rr_eq != as.raw(0)) & (rr_eq == rr_eq))

rr <- raw(1e5)
expect_true(all(and3s(rr == rr)))
expect_true(all(and3s(rr == 0L)))
expect_true(all(and3s(rr == 0)))
expect_true(all(and3s(rr %in% rr)))
expect_true(all(and3s(rr %in% 0L)))
expect_true(all(and3s(rr %in% 0)))
expect_false(any(and3s(rr %notin% rr)))
expect_false(any(and3s(rr %notin% 0L)))
expect_false(any(and3s(rr %notin% 0)))
rr <- as.raw(bb)
expect_true(all(and3s(rr == rr)))


