#include "hutilscpp.h"

SEXP Squishi(SEXP x, SEXP ab) {
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  const int a = INTEGER(ab)[0];
  const int b = INTEGER(ab)[1];
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    int xi = xp[i];
    ansp[i] = (xi <= a) ? a : ((xi >= b) ? b : xi);
  }
  UNPROTECT(1);
  return ans;
}

SEXP Squishd(SEXP x, SEXP ab) {
  R_xlen_t N = xlength(x);
  const double * xp = REAL(x);
  const double a = R_finite(REAL(ab)[0]) ? REAL(ab)[0] : R_PosInf;
  const double b = R_finite(REAL(ab)[1]) ? REAL(ab)[1] : R_NegInf;
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    double xi = xp[i];
    ansp[i] = (xi <= a) ? a : ((xi >= b) ? b : xi);
  }
  UNPROTECT(1);
  return ans;
}

SEXP CSquish(SEXP x, SEXP ab) {
  if (xlength(ab) != 2) {
    warning("xlength(ab) != 2"); // # nocov
    return x; // # nocov
  }
  if (TYPEOF(x) != TYPEOF(ab)) {
    return R_NilValue; // # nocov
  }
  switch(TYPEOF(x)) {
  case INTSXP:
    return Squishi(x, ab);
  case REALSXP:
    return Squishd(x, ab);
  }
  return R_NilValue; // # nocov
}
