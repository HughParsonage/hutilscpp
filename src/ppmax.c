#include "hutilsc.h"

SEXP do_c_pmax(SEXP x, SEXP a) {
  R_len_t nx = length(x), na = length(a);
  if (!isReal(x) || !isReal(a)) {
    error("x is not a real.");
  }
  SEXP ans = PROTECT(allocVector(REALSXP, nx));
  double *restrict ansp = REAL(ans);
  const double *ap = REAL(a);
  const double *xp = REAL(x);
  const double aaa = ap[0];
  for (int i = 0; i < nx; i++) {
    double eleme = xp[i];
    ansp[i] = eleme > aaa ? eleme : aaa;
  }
  UNPROTECT(1);
  return(ans);
}

