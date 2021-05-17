#include "hutilscpp.h"

SEXP CImplies(SEXP x, SEXP y, SEXP anyNAx, SEXP anyNAy) {
  if (TYPEOF(x) != LGLSXP ||
      TYPEOF(y) != LGLSXP) {
    error("Internal error(CImplies): x and y LGLSXP."); // # nocov
  }
  if (xlength(x) != xlength(y)) {
    error("lengths of x and y differ."); // # nocov
  }
  const bool any_nax = asLogical(anyNAx);
  const bool any_nay = asLogical(anyNAy);
  R_xlen_t N = xlength(x);
  const int * xp = LOGICAL(x);
  const int * yp = LOGICAL(y);
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);
  if (any_nax == 0 && any_nay == 0) {
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = xp[i] ? yp[i] : 1;
    }
    UNPROTECT(1);
    return ans;
  }

  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = (int)yp[i];
    if (xp[i] == NA_LOGICAL) {
      // only false results in true
      ansp[i] = yp[i] ? NA_LOGICAL : 1;
      continue;
    }
    if (yp[i] == NA_LOGICAL) {
      if (xp[i] == 0) {
        ansp[i] = 1;
      }
      continue;
    }
    if (xp[i] == 0) {
      ansp[i] = 1;
    }
  }
  UNPROTECT(1);
  return ans;
}
