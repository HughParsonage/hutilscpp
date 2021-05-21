#include "hutilscpp.h"

bool do_one_op_1_6(int op, int xi, int yi) {
  switch(op) {
  case 1:
    return xi != yi;
  case 2:
    return xi == yi;
  case 3:
    return xi >= yi;
  case 4:
    return xi <= yi;
  case 5:
    return xi >  yi;
  case 6:
    return xi <  yi;
  }
  return false; // # nocov
}

SEXP Cwhich_16(SEXP opp, SEXP x, SEXP y, SEXP nthreads) {
  if (TYPEOF(opp) != INTSXP ||
      xlength(opp) != 1 ||
      TYPEOF(x) != INTSXP ||
      TYPEOF(y) != INTSXP ||
      xlength(y) != 1 ||
      TYPEOF(nthreads) != INTSXP ||
      xlength(nthreads) != 1) {
    return R_NilValue;
  }
  int op = asInteger(opp);
  int nThread = asInteger(nthreads);
  if (op < 1 || op > 6 || nThread < 1) {
    return R_NilValue;
  }
  R_xlen_t xn = xlength(x);
  R_xlen_t yn = xlength(y);
  if (xlength(x) >= INT_MAX || xlength(y) >= INT_MAX) {
    return R_NilValue;
  }
  const bool yn1 = yn == 1;
  const bool xye = yn == xn;
  int n = (xn >= yn) ? xn : yn;

  if (xn != yn && !yn1) {
    error("Internal error(do_whichs_16): xn != yn && yn != 1"); // # nocov
  }

  const int * xp = INTEGER(x);
  const int * yp = INTEGER(y);

  SEXP out = PROTECT(allocVector(INTSXP, n));
  int * restrict outp = INTEGER(out);
  int j = 0;
  for (int i = 0; i < n; ++i) {
    const int xi = xp[i];
    const int yi = xye ? yp[i] : yp[0];
    bool resi = do_one_op_1_6(op, xi, yi);
    outp[j] = i + 1;
    j += resi;
  }
  if (j == 0) {
    UNPROTECT(1);
    return allocVector(INTSXP, 0);
  }
  SEXP o = PROTECT(allocVector(INTSXP, n));
  int * restrict oo = INTEGER(o);
  for (int i = 0; i < j; ++i) {
    oo[i] = outp[i];
  }
  UNPROTECT(2);

  return o;
}

