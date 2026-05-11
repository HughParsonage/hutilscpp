#include "hutilscpp.h"

// Phase 3 (#44): the predicate kernels live in src/vops_kernels.h, which is
// included once with VOPS_AND (this file -> vand2s_*) and once with VOPS_OR
// (Cor3s.c -> vor2s_*). This file keeps the and3s entry point, the raw
// post-AND helpers used to fold extra predicates from the R side, and the
// which() implementation on a raw mask.

// # nocov start
SEXP C_and_raw(SEXP x, SEXP y, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(x);

  if (N == 0) {
    return x;
  }
  if (xlength(y) == 0) {
    return x;
  }
  if (xlength(y) != 1 && xlength(y) != N) {
    warning("Internal error(.and_raw): y had bad length, so x will be returned.");
    return x;
  }
  if (isntRaw(y) && !isLogical(y)) {
    return y;
  }
  if (xlength(y) == 1) {
    const unsigned char y0 = isntRaw(y) ? (asLogical(y) == 1) : RAW(y)[0];
    if (y0 == 1) {
      return x;
    }
    switch(TYPEOF(x)) {
    case RAWSXP: {
      unsigned char * xp = RAW(x);
      FORLOOP(xp[i] = 0;)
    }
      break;
    case LGLSXP: {
      int * xp = LOGICAL(x);
      FORLOOP(xp[i] = 0;)
    }
      break;
    }
    return x;
  }



  switch(TYPEOF(x)) {
  case LGLSXP: {
    int * xp = LOGICAL(x);
    switch(TYPEOF(y)) {
    case LGLSXP: {
      const int * yp = LOGICAL(y);
      FORLOOP(xp[i] &= yp[i];)
    }
      break;
    case RAWSXP: {
      const unsigned char * yp = RAW(y);
      FORLOOP(xp[i] &= yp[i];)
    }
      break;
    }
  }
    break;

  case RAWSXP: {
    unsigned char * xp = RAW(x);
    switch(TYPEOF(y)) {
    case LGLSXP: {
      const int * yp = LOGICAL(y);
      FORLOOP(xp[i] &= yp[i];)
    }
      break;
    case RAWSXP: {
      const unsigned char * yp = RAW(y);
      FORLOOP(xp[i] &= yp[i];)
    }
      break;
    }
  }
    break;
  }

  return x;
}


SEXP C_or_raw(SEXP x, SEXP y, SEXP nthreads) {
  int nThread = asInteger(nthreads);
  R_xlen_t N = xlength(x);
  if (N == 0) {
    return x;
  }
  if (xlength(y) == 0) {
    return x;
  }
  if (xlength(y) != 1 && xlength(y) != N) {
    warning("Internal error(.and_raw): y had bad length, so x will be returned.");
    return x;
  }
  if (isntRaw(y) && !isLogical(y)) {
    return y;
  }
  if (xlength(y) == 1) {
    const unsigned char y0 = isntRaw(y) ? (asLogical(y) == 1) : RAW(y)[0];
    if (y0 == 0) {
      return x;
    }
    switch(TYPEOF(x)) {
    case RAWSXP: {
      unsigned char * xp = RAW(x);
      FORLOOP(xp[i] = 1;)
    }
      break;
    case LGLSXP: {
      int * xp = LOGICAL(x);
      FORLOOP(xp[i] = 1;)
    }
      break;
    }
    return x;
  }


  switch(TYPEOF(x)) {
  case LGLSXP: {
    int * xp = LOGICAL(x);
    switch(TYPEOF(y)) {
    case LGLSXP: {
      const int * yp = LOGICAL(y);
      FORLOOP(xp[i] |= yp[i];)
    }
      break;
    case RAWSXP: {
      const unsigned char * yp = RAW(y);
      FORLOOP(xp[i] |= yp[i];)
    }
      break;
    }
  }
    break;
  case RAWSXP: {
    unsigned char * xp = RAW(x);
    switch(TYPEOF(y)) {
    case LGLSXP: {
      const int * yp = LOGICAL(y);
      FORLOOP(xp[i] |= yp[i];)
    }
      break;
    case RAWSXP: {
      const unsigned char * yp = RAW(y);
      FORLOOP(xp[i] |= yp[i];)
    }
      break;
    }
  }
    break;
  }

  return x;
}
// # nocov end

#define VOPS_AND
#include "vops_kernels.h"

#define VOPS_INIT
#include "vops_kernels.h"

SEXP Cands(SEXP oo1, SEXP xx1, SEXP yy1,
           SEXP oo2, SEXP xx2, SEXP yy2,
           SEXP nthreads) {
  R_xlen_t N = xlength(xx1);
  const bool use2 = oo2 != R_NilValue;
  // # nocov start
  if (use2 && xlength(xx2) != N) {
    error("`(Cands1): xlength(xx1) = %lld`, yet `xlength(xx2) = %lld`. type '%s'",
          (long long)xlength(xx1), (long long)xlength(xx2), type2char(TYPEOF(xx2)));
  }
  // # nocov end
  int nThread = as_nThread(nthreads);

  const int o1 = sex2op(oo1);
  const int o2 = sex2op(oo2);
  if (o1 == 0 || (use2 && o2 == 0)) {
    return R_NilValue;
  }
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * ansp = RAW(ans);
  int err[1] = {0};

  // Phase 2.5: the first predicate is dispatched in INIT mode -- a
  // direct write -- so we don't pay for the previous unconditional
  // memset(ansp, 1, N) plus a kernel-side `&=` (read+modify+write).
  // The second predicate then uses the existing AND combine kernel.
  // The Phase 2 invariant survives: every COMBINE-mode kernel still
  // does `&=` against an established mask. The first-predicate INIT
  // path never reads the mask, so there's no asymmetry footgun.

  if (TYPEOF(yy1) == NILSXP) {
    switch(TYPEOF(xx1)) {
    case LGLSXP:
    {
      const int * xx1p = LOGICAL(xx1);
      if (o1 == OP_NE) {
        FORLOOP(ansp[i] = xx1p[i] != 1;)
      } else {
        FORLOOP(ansp[i] = xx1p[i] != 0;)
      }
    }
      break;
    case RAWSXP: {
      // Raw mask treated as boolean: byte == 0 is FALSE, anything else
      // is TRUE. OP_NE (i.e. `!m`) must therefore be `byte == 0`, not
      // `byte != 1` -- the latter wrongly treats truthy bytes like 2/3
      // as falsy and disagrees with the dispatcher's KFN(R), causing
      // `and3s(!m)` and `and3s(TRUE-vec, !m)` to differ for the same m.
      const unsigned char * xx1p = RAW(xx1);
      if (o1 == OP_NE) {
        FORLOOP(ansp[i] = xx1p[i] == 0;)
      } else {
        FORLOOP(ansp[i] = xx1p[i] != 0;)
      }
    }
      break;
      // # nocov start
    default: {
      error("Internal error(Cand3s): unsupported xx1 with NILSXP yy1;");
    }
    }
    // # nocov end
  } else {
    vinit2s_dispatch(ansp, o1, xx1, yy1, nThread, err);
  }
  if (use2) {
    vand2s_dispatch(ansp, o2, xx2, yy2, nThread, err);
  }
  UNPROTECT(1);
  if (err[0]) {
    return R_NilValue;
  }
  return ans;
}




SEXP C_which_raw(SEXP X, SEXP nthreads) {
#if defined _OPENMP
  int nThread = as_nThread(nthreads);
#endif
  R_xlen_t N = xlength(X);
  const unsigned char * xp = RAW(X);
  R_xlen_t o = 0, last = 0;
  if (N <= INT_MAX) {
    FORLOOP_redsum(o += xp[i] != 0;)
  } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : o) reduction(max : last)
#endif
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xp[i] != 0) {
        o++;
        last = i + 1;
      }
    }
  }
  if (last < INT_MAX) {
    SEXP ans = PROTECT(allocVector(INTSXP, o));
    int * restrict ansp = INTEGER(ans);
    int j = 0;
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[j] = i + 1;
      j += (bool)xp[i];
      if (j >= o) break;
    }
    UNPROTECT(1);
    return ans;
  }

  SEXP ans = PROTECT(allocVector(REALSXP, o));
  double * restrict ansp = REAL(ans);
  R_xlen_t j = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[j] = i + 1;
    j += (bool)xp[i];
  }
  UNPROTECT(1);
  return ans;

}
