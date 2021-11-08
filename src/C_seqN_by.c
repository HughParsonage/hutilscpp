#include "hutilscpp.h"

SEXP C_seqN_to_N(SEXP x, SEXP check) {
  R_xlen_t N = xlength(x);
  if (!isInteger(x)) {
    error("`seqN` was type '%s' but must be type integer", type2char(TYPEOF(x))); // # nocov
  }

  if (N <= 1) {
    return x;
  }

  const int * xp = INTEGER(x);
  if (!isLogical(check) || asLogical(check) != 0) {
    if (xp[0] != 1) {
      error("xp[0] != 1"); // # nocov
    }
    for (R_xlen_t i = 1; i < N; ++i) {
      if (xp[i] == 1) {
        continue;
      }
      if (xp[i] <= 0) {
        error("xp[%lld] <= 0", i + 1);
      }
      unsigned int xp0 = xp[i - 1];
      unsigned int xp1 = xp[i];
      if (xp1 - xp0 != 1) {
        error("diff != -1 at position %lld", i + 1);
      }

    }
  }
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  memcpy(ansp, xp, N * sizeof(int));
  for (R_xlen_t i = N - 2; i >= 0; --i) {
    ansp[i] = (xp[i + 1] == 1) ? ansp[i] : ansp[i + 1];
  }
  UNPROTECT(1);
  return ans;
}

