#include "hutilscpp.h"

SEXP Cwhich_true_onwards(SEXP x) {
  if (TYPEOF(x) != LGLSXP) {
    error("TYPEOF(x) != LGLSXP."); // # nocov
  }
  R_xlen_t N = xlength(x);
  R_xlen_t out = N - 1;
  const int * xp = LOGICAL(x);
  if (!xp[out]) {
    return ScalarInteger(0);
  }
  while (xp[out] && out >= 0) {
    --out;
  }
  return ScalarLength(out + 2); // out now at FALSE (or -1), + 1 for TRUE, + 1 for 0-index
}

