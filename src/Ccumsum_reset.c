#include "hutilscpp.h"

SEXP Ccumsum_reset(SEXP xx, SEXP yy) {
  R_xlen_t N = xlength(xx);
  if (TYPEOF(xx) != LGLSXP) {
    error("Internal error (Ccumsum_reset): TYPEOF(xx) != LGLSXP."); // # nocov
  }

  const int * xp = INTEGER(xx);
  if (TYPEOF(yy) == NILSXP) {
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * restrict ansp = INTEGER(ans);
    ansp[0] = xp[0] != 0;
    for (R_xlen_t i = 1; i < N; ++i) {
      // don't fall back to double
      ansp[i] = xp[i] ? (((unsigned int)ansp[i - 1]) + 1U) : 0;
    }
    UNPROTECT(1);
    return ans;
  }
  if (xlength(yy) != N) {
    error("Internal error: xlength(yy) != N."); // # nocov
  }
  if (TYPEOF(yy) == INTSXP) {
    const int * yp = INTEGER(yy);
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * restrict ansp = INTEGER(ans);
    ansp[0] = (xp[0] != 0) ? yp[0] : 0;
    for (R_xlen_t i = 1; i < N; ++i) {
      // don't fall back to double
      ansp[i] = xp[i] ? (((unsigned int)ansp[i - 1]) + yp[i]) : 0;
    }
    UNPROTECT(1);
    return ans;
  }
  if (TYPEOF(yy) == REALSXP) {
    const double * yp = REAL(yy);
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * restrict ansp = REAL(ans);
    ansp[0] = (xp[0] != 0) ? yp[0] : 0;
    for (R_xlen_t i = 1; i < N; ++i) {
      // don't fall back to double
      ansp[i] = xp[i] ? (ansp[i - 1] + yp[i]) : 0;
    }
    UNPROTECT(1);
    return ans;
  }

  return R_NilValue;
}
