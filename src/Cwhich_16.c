#include "hutilscpp.h"

bool do_one_op_1_6(int op, int xi, int yi) {
  switch(op) {
  case OP_NE:
    return xi != yi;
  case OP_EQ:
    return xi == yi;
  case OP_GE:
    return xi >= yi;
  case OP_LE:
    return xi <= yi;
  case OP_GT:
    return xi >  yi;
  case OP_LT:
    return xi <  yi;
  }
  return false; // # nocov
}

SEXP Cwhich_16(SEXP opp, SEXP x, SEXP y, SEXP nthreads) {
  int op = asInteger(opp);
  int nThread = as_nThread(nthreads);
  if (op < OP_NE || op >= OP_IN || nThread < 1) {
    return R_NilValue; // # nocov
  }
  R_xlen_t xn = xlength(x);
  R_xlen_t yn = xlength(y);
  if (xlength(x) >= INT_MAX || xlength(y) >= INT_MAX) {
    return R_NilValue; // # nocov
  }
  const bool yn1 = yn == 1;
  int n = (xn >= yn) ? xn : yn;

  const int y0 = sex2int1(y);

  if (xn != yn && !yn1) {
    error("Internal error(do_whichs_16): xn != yn && yn != 1"); // # nocov
  }

  bool op_performed = false;
  int j = 0; // length of output which(y <op> y)

  SEXP out = PROTECT(allocVector(INTSXP, n)); // intermediate output
  int * restrict outp = INTEGER(out);

  if (TYPEOF(x) == INTSXP &&
      y0 != NA_INTEGER) {
    op_performed = true;
    const int * xp = INTEGER(x);
    for (int i = 0; i < n; ++i) {
      const int xi = xp[i];
      bool resi = do_one_op_1_6(op, xi, y0);
      outp[j] = i + 1;
      j += resi;
    }
  }

  if (TYPEOF(x) == INTSXP &&
      TYPEOF(y) == INTSXP &&
      xlength(y) == n) {
    op_performed = true;
    const int * xp = INTEGER(x);
    const int * yp = INTEGER(y);

    for (int i = 0; i < n; ++i) {
      const int xi = xp[i];
      const int yi = yp[i];
      bool resi = do_one_op_1_6(op, xi, yi);
      outp[j] = i + 1;
      j += resi;
    }
  }
  if (!op_performed) {
    UNPROTECT(1);
    return R_NilValue;
  }
  if (j == 0) {
    UNPROTECT(1);
    return allocVector(INTSXP, 0);
  }
  SEXP o = PROTECT(allocVector(INTSXP, j));
  int * restrict oo = INTEGER(o);
  for (int i = 0; i < j; ++i) {
    oo[i] = outp[i];
  }
  UNPROTECT(2);
  return o;
}

