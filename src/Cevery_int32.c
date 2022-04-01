#include "hutilscpp.h"

// # nocov start
#ifdef LONG_VECTOR_SUPPORT
SEXP Cevery_int32(SEXP nthreads, SEXP Na) {
  const int na_req = asInteger(Na);
  int nThread = as_nThread(nthreads);
  SEXP ans = PROTECT(allocVector(INTSXP, 4294967296));
  int * restrict ansp = INTEGER(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (unsigned int i = 0; i < 4294967295; ++i) {
    ansp[i] = i;
  }
  ansp[4294967295] = -1;
  ansp[2147483648] = na_req;
  UNPROTECT(1);
  return ans;
}
#else
SEXP Cevery_int32(SEXP nthreads, SEXP Na) {
  return R_NilValue;
}
#endif
// # nocov end
