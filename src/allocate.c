#include "hutilscpp.h"

SEXP Callocate0_int(SEXP N, SEXP nThread) {
  if (xlength(N) != 1 || (TYPEOF(N) != INTSXP && TYPEOF(N) != REALSXP)) {
    error("N not a single number.");
  }
  if (xlength(nThread) != 1 ||
      (TYPEOF(nThread) != INTSXP && TYPEOF(nThread) != REALSXP)) {
    error("nThread not a single number.");
  }
  R_xlen_t n = TYPEOF(N) == INTSXP ? asInteger(N) : asReal(N);
  int nthreads = asInteger(nThread);
  SEXP ans = PROTECT(allocVector(INTSXP, n));
  int * restrict out = INTEGER(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nthreads)
#endif
  for (R_xlen_t i = 0; i < n; ++i) {
    out[i] = 0;
  }
  UNPROTECT(1);
  return ans;
}
