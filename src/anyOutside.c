#include "hutilscpp.h"

SEXP CanyOutside(SEXP xx, SEXP aa, SEXP bb, SEXP nasAbsent, SEXP naIsOutside) {
  // # nocov start
  if (TYPEOF(xx) != INTSXP && TYPEOF(xx) != REALSXP &&
      TYPEOF(aa) != INTSXP && TYPEOF(aa) != REALSXP &&
      TYPEOF(bb) != INTSXP && TYPEOF(bb) != REALSXP &&
      TYPEOF(nasAbsent) != LGLSXP && TYPEOF(naIsOutside) != LGLSXP) {
    error("Internal error(CanyOutside): wrong inputs types");
  }
  R_xlen_t N = xlength(xx);
  if (xlength(aa) != 1 || xlength(bb) != 1 ||
      xlength(nasAbsent) != 1 || xlength(naIsOutside) != 1) {
    error("Internal error(CanyOutside): wrong lengths.");
  }
  // # nocov end
  const int na_outside = asLogical(naIsOutside);

  switch (na_outside) {
  case -2147483648:
    // behaviour: if xpi NA then return NA; otherwise return x[i] iff
    // x[i] < a || x[i] > b
    if (TYPEOF(xx) == INTSXP) {
      const int * xp = INTEGER(xx);
      const int a = asInteger(aa);
      const int b = asInteger(bb);
      for (R_xlen_t i = 0; i < N; ++i) {
        int xpi = xp[i];
        if (xpi == NA_INTEGER) {
          return ScalarInteger(NA_INTEGER);
        }
        if (xpi < a || xpi > b) {
          return ScalarLength(i + 1);
        }
      }
      return ScalarInteger(0);
    } else {
      const double * xp = REAL(xx);
      const double a = asReal(aa);
      const double b = asReal(bb);
      for (R_xlen_t i = 0; i < N; ++i) {
        double xpi = xp[i];
        if (ISNAN(xpi)) {
          return ScalarInteger(NA_INTEGER);
        }
        if (xpi < a || xpi > b) {
          return ScalarLength(i + 1);
        }
      }
      return ScalarInteger(0);
    }

    break;
  case 0:
    // behaviour: if xpi NA then skip; otherwise return x[i] iff
    // x[i] < a || x[i] > b
    if (TYPEOF(xx) == INTSXP) {
      const int * xp = INTEGER(xx);
      const int a = asInteger(aa);
      const int b = asInteger(bb);
      for (R_xlen_t i = 0; i < N; ++i) {
        int xpi = xp[i];
        if (xpi == NA_INTEGER) {
          continue;
        }
        if (xpi < a || xpi > b) {
          return ScalarLength(i + 1);
        }
      }
      return ScalarInteger(0);
    } else {
      const double * xp = REAL(xx);
      const double a = asReal(aa);
      const double b = asReal(bb);
      for (R_xlen_t i = 0; i < N; ++i) {
        double xpi = xp[i];
        if (ISNAN(xpi)) {
          continue;
        }
        if (xpi < a || xpi > b) {
          return ScalarLength(i + 1);
        }
      }
      return ScalarInteger(0);
    }
    break;
  case 1:
    // behaviour: if xpi NA return i + 1  then skip; otherwise return i + 1 iff
    // x[i] < a || x[i] > b
    if (TYPEOF(xx) == INTSXP) {
      const int * xp = INTEGER(xx);
      const int a = asInteger(aa);
      const int b = asInteger(bb);
      for (R_xlen_t i = 0; i < N; ++i) {
        int xpi = xp[i];
        // xpi == NA_INTEGER => xpi < a for all non NA a
        // but a == NA_INTEGER implied unbounded below
        if (xpi < a || xpi > b) {
          return ScalarLength(i + 1);
        }
      }
      return ScalarInteger(0);
    } else {
      const double * xp = REAL(xx);
      const double a = asReal(aa);
      const double b = asReal(bb);
      for (R_xlen_t i = 0; i < N; ++i) {
        double xpi = xp[i];
        if (ISNAN(xpi) || xpi < a || xpi > b) {
          return ScalarLength(i + 1);
        }
      }
      return ScalarInteger(0);
    }
    break;
  }
  return R_NilValue; // # nocov
}



