#include "hutilscpp.h"

unsigned int pcg_hash(unsigned int input)
{
  unsigned int state = input * 747796405u + 2891336453u;
  unsigned int word = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
  return (word >> 22u) ^ word;
}


SEXP Cpcg_hash(SEXP n, SEXP r, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  if (nThread > 32) {
    nThread = 32;
  }
  unsigned int N = asReal(n);

  unsigned int States[32] = {};
  if (TYPEOF(r) == INTSXP && xlength(r) >= 32) {
    for (int i = 0; i < 32; ++i) {
      States[i] = INTEGER_ELT(r, i);
    }
  } else {
    for (int i = 0; i < 32; ++i) {
      States[i] = i + 2U;
    }
  }
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (unsigned int i = 0; i < N; ++i) {
#ifdef _OPENMP
    int oi = omp_get_thread_num();
#else
    int oi = (i & 31U);
#endif
    unsigned int new_si = pcg_hash(States[oi]);
    ansp[i] = new_si;
    States[oi] = new_si;
  }
  UNPROTECT(1);
  return ans;
}

SEXP firstAbsentInt(SEXP xx, SEXP From, SEXP nthreads) {
  if (TYPEOF(xx) != INTSXP) {
    error("Not an int.");
  }
  R_xlen_t N = xlength(xx);
  int nThread = as_nThread(nthreads);
  if (nThread > 32) {
    nThread = 32;
  }
  const int * xp = INTEGER(xx);
  unsigned char * all_ints = calloc(4294967296, sizeof(char));
  if (all_ints == NULL) {
    free(all_ints);
    warning("Unable to allocate.");
    return R_NilValue;
  }


#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int ui = xp[i];
    all_ints[ui] += 1;
  }
  unsigned int maxi[256] = {0};

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : maxi[:256])
#endif
  for (unsigned int i = 0; i < 4294967295U; ++i) {
    unsigned char aii = all_ints[i];
    unsigned int uii = aii;
    maxi[uii] += 1;
  }
  free(all_ints);
  SEXP ans = PROTECT(allocVector(INTSXP, 256));
  int * ansp = INTEGER(ans);
  for (int i = 0; i < 256; ++i) {
    ansp[i] = maxi[i];
  }
  UNPROTECT(1);
  return ans;
}

