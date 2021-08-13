#include "hutilscpp.h"


static const int implies_result[9] = {1, 1, 0, 1,
                                   NA_INT, NA_INT,
                                   1, 1, NA_INT};

inline int na2two(int x) {
  return x == NA_INTEGER ? 2 : x;
}

inline int do_implies(int x, int y) {
  return implies_result[na2two(x) + 3 * na2two(y)];
}

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
    int xpi = xp[i];
    int ypi = yp[i];
    ansp[i] = do_implies(xpi, ypi);

  }
  UNPROTECT(1);
  return ans;
}
