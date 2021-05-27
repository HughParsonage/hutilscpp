#include "hutilscpp.h"

SEXP Cpar_in_int(SEXP xx, SEXP yy, SEXP nthreads) {
  R_xlen_t N = xlength(xx);
  if (TYPEOF(xx) != INTSXP) {
    error("Internal error(Cpar_in_int): TYPEOF(xx) != INTSXP."); // # nocov
  }
  if (xlength(yy) > INT_MAX) {
    error("xlength(yy) > INT_MAX"); // # nocov
  }
  int tn = xlength(yy);
  const int * xp = INTEGER(xx);
  const int * yp = INTEGER(yy);
  int nThread = as_nThread(nthreads);
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    int oi = 0;
    int xi = xp[i];
    for (int j = 0; j < tn; ++j) {
      if (yp[j] == xi) {
        oi = 1;
        break;
      }
    }
    ansp[i] = oi;
  }
  UNPROTECT(1);
  return ans;

}
