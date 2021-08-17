#include "hutilscpp.h"


static const int implies_result[9] = {1, 1, 1, 0, 1,
                                      NA_INT, NA_INT,
                                      1, NA_INT};

unsigned int na2two(int x) {
  return x < 0 ? 2 : x;
}

int do_implies(int x, int y) {
  return implies_result[na2two(y) + 3 * na2two(x)];
}

SEXP CImplies(SEXP x, SEXP y, SEXP anyNAx, SEXP anyNAy) {
  if (TYPEOF(x) != LGLSXP ||
      TYPEOF(y) != LGLSXP) {
    error("Internal error(CImplies): x and y LGLSXP."); // # nocov
  }
  if (xlength(x) != xlength(y)) {
    error("lengths of x and y differ."); // # nocov
  }
  R_xlen_t N = xlength(x);
  const int * xp = LOGICAL(x);
  const int * yp = LOGICAL(y);
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    int ypi = yp[i];
    ansp[i] = do_implies(xpi, ypi);

  }
  UNPROTECT(1);
  return ans;
}
