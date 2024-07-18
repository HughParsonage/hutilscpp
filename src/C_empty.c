#include "hutilscpp.h"

static bool iempty(const int * xp, R_xlen_t N, bool expected, int nThread) {
  if (xp[0] != NA_INTEGER) {
    return false;
  }
  uint32_t NN = (unsigned int)N;
  for (uint32_t i = 1; i < NN; i <<= 2) {
    if (xp[i] != NA_INTEGER) {
      return false;
    }
  }

  if (expected) {
    bool o = true;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(&& : o)
#endif
    for (R_xlen_t i = 1; i < N; ++i) {
      if (xp[i] != NA_INTEGER) {
        o = false;
      }
    }
    return o;
  }
  for (R_xlen_t i = 1; i < N; ++i) {
    if (xp[i] != NA_INTEGER) {
      return false;
    }
  }
  return true;
}

static bool dempty(const double * xp, R_xlen_t N, const bool expected, int nThread) {
  if (!ISNAN(xp[0])) {
    return false;
  }
  if (expected) {
    bool o = true;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(&& : o)
#endif
    for (R_xlen_t i = 1; i < N; ++i) {
      o &= ISNAN(xp[i]);
    }
    return o;
  }
  for (R_xlen_t i = 1; i < N; ++i) {
    if (!ISNAN(xp[i])) {
      return false;
    }
  }
  return true;
}

static bool sempty(const SEXP * xp, R_xlen_t N) {
  if (xp[0] != NA_STRING) {
    return false;
  }
  for (R_xlen_t i = 1; i < N; ++i) {
    if (xp[i] != NA_STRING) {
      return false;
    }
  }
  return true;
}

static bool cempty(const Rcomplex * xp, R_xlen_t N) {
  for (R_xlen_t j = 0; j < N; ++j) {
    double rj = xp[j].r;
    double ij = xp[j].i;
    if (ISNAN(rj) || ISNAN(ij)) {
      continue;
    }
    return false;
  }
  return true;
}

static bool empty(SEXP x, bool e, int nThread) {
  R_xlen_t N = xlength(x);
  switch(TYPEOF(x)) {
  case LGLSXP:
    return iempty(LOGICAL(x), N, e, nThread);
  case INTSXP:
    return iempty(INTEGER(x), N, e, nThread);
  case REALSXP:
    return dempty(REAL(x), N, e, nThread);
  case STRSXP:
    return sempty(STRING_PTR_RO(x), N);
  case CPLXSXP:
    return cempty(COMPLEX(x), N);
  case RAWSXP:
    return false;
  }
  return false;
}

SEXP C_empty(SEXP x, SEXP E, SEXP L0, SEXP nthreads) {
  if (xlength(x) == 0) {
    return L0;
  }
  int nThread = as_nThread(nthreads);
  const bool e = asLogical(E);
  return ScalarLogical(empty(x, e, nThread));
}
