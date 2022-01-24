#include "hutilscpp.h"

SEXP Craw2lgl(SEXP x, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  if (isLogical(x)) {
    return x;
  }
  if (isntRaw(x)) {
    error("`x` was type '%s' but must be raw.", type2char(TYPEOF(x)));
  }
  R_xlen_t N = xlength(x);
  const unsigned char * xp = RAW(x);
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = xp[i];
  }
  UNPROTECT(1);
  return ans;
}
