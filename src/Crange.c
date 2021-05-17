#include "hutilscpp.h"

SEXP Crangel4(SEXP x) {
  R_xlen_t N = xlength(x);
  if (N == 0) {
    SEXP ans = PROTECT(allocVector(INTSXP, 4));
    INTEGER(ans)[0] = 1;
    INTEGER(ans)[1] = 0;
    INTEGER(ans)[2] = 0;
    INTEGER(ans)[3] = 0;
    UNPROTECT(1);
    return ans;
  }
  const int * xp = LOGICAL(x);
  bool has_true = false;
  bool has_false = false;
  R_xlen_t wmin = 1, wmax = 1;
  for (R_xlen_t i = 0; i < N; ++i) {
    if (has_true & has_false) {
      break;
    }
    switch(xp[i]) {
    case 0: {
      wmin = has_false ? wmin : i + 1;
      has_false = true;
    }
      break;
    case 1: {
      wmax = has_true ? wmax : i + 1;
      has_true = true;
    }
      break;
    }
  }

  if (wmin <= INT_MAX && wmax <= INT_MAX) {
    SEXP ans = PROTECT(allocVector(INTSXP, 4));
    if (!has_true && !has_false) {
      // empty
      INTEGER(ans)[0] = NA_LOGICAL;
      INTEGER(ans)[1] = NA_LOGICAL;
      INTEGER(ans)[2] = NA_LOGICAL;
      INTEGER(ans)[3] = NA_LOGICAL;
    } else {
      INTEGER(ans)[0] = has_false ? 0 : 1;
      INTEGER(ans)[1] = has_true ? 1 : 0;
      INTEGER(ans)[2] = wmin;
      INTEGER(ans)[3] = wmax;
    }
    UNPROTECT(1);
    return ans;
  }
  SEXP ans = PROTECT(allocVector(REALSXP, 4));
  REAL(ans)[0] = has_false ? 0 : 1;
  REAL(ans)[1] = has_true ? 1 : 0;
  REAL(ans)[2] = wmin;
  REAL(ans)[3] = wmax;
  UNPROTECT(1);
  return ans;
}

SEXP Crangei4(SEXP x) {
  R_xlen_t N = xlength(x);
  if (N == 0) {
    SEXP ans = PROTECT(allocVector(INTSXP, 4));
    INTEGER(ans)[0] = INT_MAX;
    INTEGER(ans)[1] = INT_MIN;
    INTEGER(ans)[2] = 0;
    INTEGER(ans)[3] = 0;
    UNPROTECT(1);
    return ans;
  }
  const int * xp = INTEGER(x);
  int xmin = xp[0], xmax = xp[0];
  R_xlen_t wmin = 1, wmax = 1;
  while (xmin == NA_INTEGER && wmin < N) {
    xmin = xp[wmin];
    ++wmin;
    ++wmax;
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    if (xpi < xmin) {
      if (xpi == NA_INTEGER) {
        continue;
      }
      wmin = i + 1;
      xmin = xpi;
    } else if (xpi > xmax) {
      wmax = i + 1;
      xmax = xpi;
    }
  }
  if (wmin <= INT_MAX && wmax <= INT_MAX) {
    SEXP ans = PROTECT(allocVector(INTSXP, 4));
    INTEGER(ans)[0] = xmin;
    INTEGER(ans)[1] = xmax;
    INTEGER(ans)[2] = wmin;
    INTEGER(ans)[3] = wmax;
    UNPROTECT(1);
    return ans;
  }
  SEXP ans = PROTECT(allocVector(REALSXP, 4));
  REAL(ans)[0] = xmin;
  REAL(ans)[1] = xmax;
  REAL(ans)[2] = wmin;
  REAL(ans)[3] = wmax;
  UNPROTECT(1);
  return ans;
}

SEXP Cranged4(SEXP x) {
  R_xlen_t N = xlength(x);
  if (N == 0) {
    SEXP ans = PROTECT(allocVector(REALSXP, 4));
    REAL(ans)[0] = R_PosInf;
    REAL(ans)[1] = R_NegInf;
    REAL(ans)[2] = 0;
    REAL(ans)[3] = 0;
    UNPROTECT(1);
    return ans;
  }
  const double * xp = REAL(x);
  double xp0 = xp[0];
  R_xlen_t j = 1;
  while (ISNAN(xp0) && j < N) {
    xp0 = xp[j];
    ++j;
  }
  double xmin = xp0, xmax = xp0;

  R_xlen_t wmin = j, wmax = j;
  for (R_xlen_t i = 0; i < N; ++i) {
    double xpi = xp[i];
    if (ISNAN(xpi)) {
      continue;
    }
    if (xpi < xmin) {
      wmin = i + 1;
      xmin = xpi;
    } else if (xpi > xmax) {
      wmax = i + 1;
      xmax = xpi;
    }
  }
  SEXP ans = PROTECT(allocVector(REALSXP, 4));
  REAL(ans)[0] = xmin;
  REAL(ans)[1] = xmax;
  REAL(ans)[2] = wmin;
  REAL(ans)[3] = wmax;
  UNPROTECT(1);
  return ans;
}

SEXP Crange(SEXP x) {
  switch(TYPEOF(x)) {
  case NILSXP:
    return R_NilValue;
  case LGLSXP:
    return Crangel4(x);
  case INTSXP:
    return Crangei4(x);
  case REALSXP:
    return Cranged4(x);
  }
  return R_NilValue;
}
