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

  return R_NilValue; // # nocov
}

// # nocov start
SEXP Ccumsum_reset_where(SEXP xx, SEXP yy, SEXP oo, SEXP aa) {
  R_xlen_t N = xlength(xx);
  if (N != xlength(yy) || N == 0) {
    error("Internal error(Ccumsum_reset_where): length(x) != length(y)");
  }
  if (TYPEOF(aa) != INTSXP || xlength(aa) == 0) {
    error("Internal error(Ccumsum_reset_where): aa wrong type or length.");
  }
  const int o = asInteger(oo);

  if (TYPEOF(xx) == INTSXP) {
    const int a1 = asInteger(aa);
    const int a2 = xlength(aa) == 1 ? INTEGER(aa)[0] : INTEGER(aa)[1];
    const int * x = INTEGER(xx);
    const int * y = INTEGER(yy);
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * restrict ansp = INTEGER(ans);
    ansp[0] = isingle_ox_x1_x2(x[0], o, a1, a2) ? 0 : y[0];
    for (R_xlen_t i = 1; i < N; ++i) {
      bool qi = (x[i] == NA_INTEGER) || isingle_ox_x1_x2(x[i], o, a1, a2);
      ansp[i] = qi ? ((unsigned int)y[i] + ansp[i - 1]) : 0;
    }
    UNPROTECT(1);
    return ans;
  }
  if (TYPEOF(xx) == REALSXP &&
      TYPEOF(yy) == INTSXP &&
      TYPEOF(aa) == REALSXP) {
    const double a1 = asInteger(aa);
    const double a2 = xlength(aa) == 1 ? REAL(aa)[0] : REAL(aa)[1];
    const double * x = REAL(xx);
    const int * y = INTEGER(yy);
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * restrict ansp = INTEGER(ans);
    ansp[0] = isingle_ox_x1_x2(y[0], o, a1, a2) ? 0 : y[0];
    for (R_xlen_t i = 1; i < N; ++i) {
      bool qi = (x[i] == NA_INTEGER) || dsingle_ox_x1_x2(x[i], o, a1, a2);
      ansp[i] = qi ? ((unsigned int)y[i] + ansp[i - 1]) : 0;
    }
    UNPROTECT(1);
    return ans;
  }
  return R_NilValue;
}
// # nocov end


SEXP Ccumsum_reset_sorted_int(SEXP xx) {
  R_xlen_t N = xlength(xx);
  if (TYPEOF(xx) != INTSXP) {
    error("Internal error(Ccumsum_reset): xx not INTSXP."); // # nocov
  }
  const int * xp = INTEGER(xx);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  ansp[0] = 1;
  for (R_xlen_t i = 1; i < N; ++i) {
    ansp[i] = (xp[i] == xp[i - 1]) ? (ansp[i - 1] + 1U) : 1;
  }
  UNPROTECT(1);
  return ans;
}

