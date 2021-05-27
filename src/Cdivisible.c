#include "hutilscpp.h"

const bool ON_TWOS_COMPLEMENT = (-1 == ~0);

SEXP Cdivisible(SEXP xx, SEXP dd, SEXP nthreads) {
  R_xlen_t N = xlength(xx);
  int nThread = as_nThread(nthreads);

  if (TYPEOF(xx) != INTSXP) {
    error("Internal error(Cdivisible): xx not INTSXP."); // # nocov
  }
  const int * xp = INTEGER(xx);
  const unsigned int d = asInteger(dd);
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xi = xp[i];
    ansp[i] = !(xi % d);
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cdivisible2(SEXP xx, SEXP nthreads, SEXP KeepNas) {
  R_xlen_t N = xlength(xx);
  // # nocov start
  if (!ON_TWOS_COMPLEMENT) {
    error("Unexpected arch.");
  }

  if (TYPEOF(xx) != INTSXP && TYPEOF(xx) != REALSXP) {
    error("Internal error(Cdivisible2): xx not INTSXP/REALSXP.");
  }
  if (TYPEOF(KeepNas) != LGLSXP || xlength(KeepNas) != 1) {
    error("Internal error(Cdivisible2): KeepNas not TF.");
  }
  // # nocov end

  int nThread = as_nThread(nthreads);
  const bool keep_na = asLogical(KeepNas);
  SEXP out = PROTECT(allocVector(LGLSXP, N));
  int * restrict outp = LOGICAL(out);

if (TYPEOF(xx) == INTSXP) {
  const int * xp = INTEGER(xx);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) schedule(static)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    if (keep_na && xp[i] == NA_INTEGER) {
      outp[i] = NA_LOGICAL;
      continue;
    }
    unsigned int ui = xp[i];
    outp[i] = !(ui & 1U);
  }
} else {
  const double * xp = REAL(xx);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) schedule(static)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    if (keep_na && !R_finite(xp[i])) {
      outp[i] = NA_LOGICAL;
      continue;
    }
    double di = xp[i];
    outp[i] = fmod(di, 2) == 0;
  }
}
  UNPROTECT(1);
  return out;
}

SEXP Cdivisible16(SEXP xx, SEXP nthreads) {
  R_xlen_t N = xlength(xx);
  if (TYPEOF(xx) != INTSXP) {
    error("Internal error(Cdivisible16): xx not INTSXP."); // # nocov
  }
  SEXP out = PROTECT(allocVector(LGLSXP, N));
  int * restrict outp = LOGICAL(out);
  const int * xp = INTEGER(xx);
  int nThread = as_nThread(nthreads);

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xi = (unsigned int)xp[i];
    outp[i] = !(xi & 15U);
  }
  UNPROTECT(1);
  return out;
}
