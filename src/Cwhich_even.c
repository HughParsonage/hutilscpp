#include "hutilscpp.h"

SEXP Cwhich_even(SEXP xx) {
  R_xlen_t NN = xlength(xx);
  if (TYPEOF(xx) != INTSXP &&
      TYPEOF(xx) != REALSXP) {
    error("Internal error(Cwhich_even): non-numeric xx."); // # nocov
  }
  if (NN > INT_MAX) {
    error("Internal error: long vectors are not supported."); // # nocov
  }
  int N = (int)NN;
  int n_even = 0;
  if (TYPEOF(xx) == INTSXP) {
    // Just count number of even
    const int * xp = INTEGER(xx);
    for (int i = 0; i < N; ++i) {
      n_even += !(((unsigned int)xp[i]) & 1U);
    }

  } else {
    const double * xp = REAL(xx);
    for (int i = 0; i < N; ++i) {
      n_even += R_finite(xp[i]) && (fmod(xp[i], 2) == 0);
    }
  }
  if (n_even == 0) {
    return allocVector(INTSXP, 0);
  }
  SEXP ans = PROTECT(allocVector(INTSXP, n_even));
  int * restrict ansp = INTEGER(ans);
  if (TYPEOF(xx) == INTSXP) {
    const int * xp = INTEGER(xx);
    for (R_xlen_t i = 0, j = 0; i < N; ++i) {
      int is_even = !(((unsigned int)xp[i]) & 1U);
      ansp[j] = (int)(i + 1);
      j += is_even;
    }
  } else {
    const double * xp = REAL(xx);
    for (R_xlen_t i = 0, j = 0; i < N; ++i) {
      int is_even = R_finite(xp[i]) && (fmod(xp[i], 2) == 0);
      ansp[j] = (int)(i + 1);
      j += is_even;
    }
  }
  UNPROTECT(1);
  return ans;
}
