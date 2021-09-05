#include "hutilscpp.h"

SEXP Ccount_logical(SEXP xx, SEXP nthreads) {
  if (TYPEOF(xx) != LGLSXP) {
    error("Internal error(Ccount_logical): Wrong types passed."); // # nocov
  }
  int nThread = as_nThread(nthreads);
  R_xlen_t n = xlength(xx);
  R_xlen_t trues = 0;
  R_xlen_t nas = 0;
  const int * xp = LOGICAL(xx);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+:trues,nas)
#endif
  for (R_xlen_t i = 0; i < n; ++i) {
    if (xp[i] == NA_LOGICAL) {
      nas += 1;
    } else if (xp[i]) {
      trues += 1;
    }
  }
  R_xlen_t falses = n - trues - nas;
  SEXP o = PROTECT(allocVector(REALSXP, 3));
  REAL(o)[0] = falses;
  REAL(o)[1] = trues;
  REAL(o)[2] = nas;
  UNPROTECT(1);
  return o;
}
