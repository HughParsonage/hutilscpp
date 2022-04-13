#include "hutilscpp.h"

#ifdef __SIZEOF_INT128__
__uint128_t g_lehmer64_state;

void reset_state(const int * rp, R_xlen_t N) {
  __uint128_t o = 1337;
  for (R_xlen_t i = 0; i < N; ++i) {
    o += rp[i];
    o <<= 27;
  }
  g_lehmer64_state = o;
}

uint64_t lehmer64() {
  g_lehmer64_state *= 0xda942042e4dd58b5;
  return g_lehmer64_state >> 64;
}

SEXP C_pcg_hash(SEXP NN, SEXP RR) {
  if ((!isInteger(NN) && !isReal(NN)) || (!isInteger(RR) && !isNull(RR))) {
    return R_NilValue; // # nocov
  }
  R_xlen_t N = isReal(NN) ? asReal(NN) : asInteger(NN);
  if (isInteger(RR)) {
    R_xlen_t nr = xlength(RR);
    const int * rp = INTEGER(RR);
    reset_state(rp, nr);
  }
  uint64_t l64 = lehmer64();
  if (l64 == 0) {
    l64 = 2891336453;
  }

  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 2; i <= N; i += 2) {
    uint64_t rii = lehmer64();
    ansp[i - 2] = rii & 4294967295;
    ansp[i - 1] = rii >> 32;
  }
  if ((N % 2)) {
    ansp[N - 1] = lehmer64() & 4294967295;
  }
  UNPROTECT(1);
  return ans;
}

#else
SEXP C_pcg_hash(SEXP NN, SEXP RR) {
  return R_NilValue;
}
#endif
