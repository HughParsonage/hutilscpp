#include "hutilscpp.h"

SEXP Clgl2raw(SEXP x, SEXP Na, SEXP nthreads) {
  R_xlen_t N = xlength(x);
  const int * xp = LOGICAL(x);
  int nThread = as_nThread(nthreads);
  unsigned char na = asInteger(Na) & 255;
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * ansp = RAW(ans);
  switch(na) {
  case 0:
    FORLOOP(ansp[i] = xp[i] == 1;)
    break;
  default:
    FORLOOP(ansp[i] = xp[i] == NA_LOGICAL ? na : xp[i] == 1;)
  }
  UNPROTECT(1);
  return ans;
}
