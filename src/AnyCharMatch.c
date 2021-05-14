#include "hutilscpp.h"

SEXP CAnyCharMatch(SEXP X, SEXP A, SEXP Opposite) {
  R_xlen_t N = xlength(X);
  R_xlen_t M = xlength(A);
  R_xlen_t o = 0;
  if (TYPEOF(X) != STRSXP || TYPEOF(A) != STRSXP) {
    error("Internal error: X or A not string."); // # nocov
  }
  const bool opposite = asLogical(Opposite);
  for (R_xlen_t i = 0; i < N; ++i) {
    R_xlen_t j = (M == N) ? i : 0;
    const int xilen = length(STRING_ELT(X, i));
    const int ailen = length(STRING_ELT(A, j));
    const char * xi = CHAR(STRING_ELT(X, i));
    const char * yi = CHAR(STRING_ELT(A, j));

    if (opposite) {
      if (xilen != ailen) {
        o = i + 1;
        break;
      }
      for (int c = 0; c < xilen; ++c) {
        if (xi[c] != yi[c]) {
          o = i + 1;
          break;
        }
      }
    } else {
      if (xilen != ailen) {
        continue;
      }
      bool matched = true;
      for (int c = 0; c < xilen; ++c) {
        if (xi[c] != yi[c]) {
          matched = false;
          break;
        }
      }
      if (matched) {
        o = i + 1;
        break;
      }
    }
  }
  return o < INT_MAX ? ScalarInteger(o) : ScalarReal(o);
}
