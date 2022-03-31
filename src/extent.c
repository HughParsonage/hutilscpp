#include "hutilscpp.h"

unsigned int iextent(const int * xp, R_xlen_t N, int nThread, int * aminmax) {
  int amin = xp[0], amax = xp[0];
  if (N <= 1048576) {
    nThread = 1;
  }

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(min : amin) reduction(max : amax)
#endif
  for (R_xlen_t i = 1; i < N; ++i) {
    amin = minii(xp[i], amin);
    amax = maxii(xp[i], amax);
  }
  aminmax[0] = amin;
  aminmax[1] = amax;
  unsigned int o = (int64_t)amax - (int64_t)amin;
  return o;
}

bool thinner(const int * xp, R_xlen_t N, int nThread, unsigned int width, int * aminmax) {
  return iextent(xp, N, nThread, aminmax) <= width;
}

SEXP Cextent(SEXP x, SEXP nthreads) {
  R_xlen_t N = xlength(x);
  int nThread = as_nThread(nthreads);
  int aminmax[2] = {0};
  switch(TYPEOF(x)) {
  case INTSXP:
    return ScalarLength(iextent(INTEGER(x), N, nThread, aminmax));
  }
  return R_NilValue;
}
