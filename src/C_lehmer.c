#include "hutilscpp.h"

#ifdef __SIZEOF_INT128__
__uint128_t g_lehmer64_state;
__uint128_t g_lehmer64_states[8] = {192210860, 1419112641, 1887327538, 1182732464, 765692391, 308751291, 2034556665, 223394413};

static void reset_state(const int * rp, R_xlen_t N) {
  __uint128_t o = 1337;
  for (R_xlen_t i = 0; i < N; ++i) {
    o += rp[i];
    o <<= 27;
    o += 3;
  }
  g_lehmer64_state = o;
}

static uint64_t lehmer64(void) {
  g_lehmer64_state *= 0xda942042e4dd58b5;
  return g_lehmer64_state >> 64;
}

static uint64_t plehmer64(int t) {
  g_lehmer64_states[t] *= 0xda942042e4dd58b5;
  return g_lehmer64_states[t] >> 64;
}

static void reset_states(const int * rp, R_xlen_t N) {
  for (int i = 0; i < 8; ++i) {
    g_lehmer64_states[i] = 7 + lehmer64();
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    g_lehmer64_states[i % 8] += rp[i];
    g_lehmer64_states[i % 8] *= 1337;
  }
}

SEXP C_lehmer64(SEXP NN, SEXP RR) {
  if ((!isInteger(NN) && !isReal(NN)) || (!isInteger(RR) && !isNull(RR))) {
    return R_NilValue; // # nocov
  }
  R_xlen_t N = isReal(NN) ? asReal(NN) : asInteger(NN);
  if (isInteger(RR)) {
    R_xlen_t nr = xlength(RR);
    const int * rp = INTEGER(RR);
    reset_state(rp, nr);
  }
  while ((g_lehmer64_state >> 32) == 0) {
    g_lehmer64_state += 3;
    g_lehmer64_state *= 2891336453;
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

SEXP C_lehmer64_par(SEXP NN, SEXP RR, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  if (nThread > 8) {
    nThread = 8;
  }

  if ((!isInteger(NN) && !isReal(NN)) || (!isInteger(RR) && !isNull(RR))) {
    return R_NilValue; // # nocov
  }
  R_xlen_t N = isReal(NN) ? asReal(NN) : asInteger(NN);
  if (isInteger(RR)) {
    R_xlen_t nr = xlength(RR);
    const int * rp = INTEGER(RR);
    reset_states(rp, nr);
  }

  // Populate the (likely zero) states
  for (int k = 0; k < 8; ++k) {
    __uint128_t xk = g_lehmer64_states[k];
    int wnk = 0;
    while (++wnk <= 1234567 && (xk >> 64) == 0) {
      xk += 3;
      xk *= 2891336453;
    }
    g_lehmer64_states[k] = xk;
  }

  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 2; i <= N; i += 2) {
#if defined _OPENMP && _OPENMP >= 201511
    int t = omp_get_thread_num();
#else
    int t = 0;
#endif
    uint64_t rii = plehmer64(t & 7);
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
SEXP C_lehmer64(SEXP NN, SEXP RR) {
  return R_NilValue;
}
SEXP C_lehmer64_par(SEXP NN, SEXP RR, SEXP nthreads) {
  return R_NilValue;
}
#endif

SEXP C__alloc(SEXP NN) {
  R_xlen_t N = isReal(NN) ? asReal(NN) : asInteger(NN);
  return allocVector(INTSXP, N);
}

