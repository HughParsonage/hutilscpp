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
    int xi = xp[i];
    bool nochange = xi >= amin && xi <= amax;
    if (nochange) continue;
    amin = minii(xi, amin);
    amax = maxii(xi, amax);
  }
  aminmax[0] = amin;
  aminmax[1] = amax;
  unsigned int o = (int64_t)amax - (int64_t)amin;
  return o;
}

bool ithinner(const int * xp, R_xlen_t N, int nThread, unsigned int width, int * aminmax) {
  return iextent(xp, N, nThread, aminmax) <= width;
}


