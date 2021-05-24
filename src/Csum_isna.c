#include "hutilscpp.h"

R_xlen_t sum_isna_int(const int * x, R_xlen_t N, int nThread) {
  R_xlen_t o = 0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : o)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    o += x[i] == NA_INTEGER;
  }
  return o;
}

R_xlen_t sum_isna_dbl(const double * x, R_xlen_t N, int nThread) {
  R_xlen_t o = 0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : o)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    o += ISNAN(x[i]);
  }
  return o;
}

R_xlen_t sum_isna_chr(SEXP x) {
  R_xlen_t N = xlength(x);
  R_xlen_t o = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    o += STRING_ELT(x, i) == NA_STRING;
  }
  return o;
}

R_xlen_t sum_isna_complx(SEXP x) {
  R_xlen_t N = xlength(x);
  R_xlen_t o = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    Rcomplex v = COMPLEX_ELT(x, i);
    if (ISNAN(v.r) || ISNAN(v.i)) {
      o += 1;
    }
  }
  return o;
}

R_xlen_t sum_isna(SEXP x, SEXP nthreads) {
  R_xlen_t N = xlength(x);
  int nThread = as_nThread(nthreads);
  switch(TYPEOF(x)) {
  case LGLSXP:
    return sum_isna_int(LOGICAL(x), N, nThread);
  case INTSXP:
    return sum_isna_int(INTEGER(x), N, nThread);
  case REALSXP:
    return sum_isna_dbl(REAL(x), N, nThread);
  case STRSXP:
    return sum_isna_chr(x);
  case CPLXSXP:
    return sum_isna_complx(x);
  }
  return 0;
}

SEXP Csum_isna(SEXP x, SEXP nthreads) {
  return ScalarLength(sum_isna(x, nthreads));
}



