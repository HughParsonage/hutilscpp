#include "hutilscpp.h"

static inline bool is_even_int(int x) {
  return !((unsigned int)x & 1U);
}
static inline bool is_even_dbl(double x) {
  return R_finite(x) && (fmod(x, 2) == 0);
}

SEXP Cwhich_even(SEXP xx) {
  R_xlen_t NN = xlength(xx);
  if (TYPEOF(xx) != INTSXP &&
      TYPEOF(xx) != REALSXP) {
    error("Internal error(Cwhich_even): non-numeric xx."); // # nocov
  }
  if (NN >= INT_MAX) {
    error("Internal error: long vectors are not supported."); // # nocov
  }
  int N = (int)NN;
  int n_even = 0;
  if (TYPEOF(xx) == INTSXP) {
    // Just count number of even
    const int * xp = INTEGER(xx);
    for (int i = 0; i < N; ++i) {
      n_even += is_even_int(xp[i]);
    }

  } else {
    const double * xp = REAL(xx);
    for (int i = 0; i < N; ++i) {
      n_even += is_even_dbl(xp[i]);
    }
  }
  if (n_even == 0) {
    return allocVector(INTSXP, 0);
  }
  SEXP ans = PROTECT(allocVector(INTSXP, n_even));
  int * restrict ansp = INTEGER(ans);
  if (TYPEOF(xx) == INTSXP) {
    const int * xp = INTEGER(xx);
    for (int i = 0, j = 0; (i < N && j < n_even); ++i) {
      ansp[j] = ((unsigned int)i) + 1U;
      j += is_even_int(xp[i]);
    }
  } else {
    const double * xp = REAL(xx);
    for (int i = 0, j = 0; (i < N && j < n_even); ++i) {
      ansp[j] = ((unsigned int)i) + 1U;
      j += is_even_dbl(xp[i]);
    }
  }
  UNPROTECT(1);
  return ans;
}
