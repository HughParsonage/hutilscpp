#include "hutilscpp.h"

#define SUM_ISNA_REGION_SIZE 4096

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

#if defined(R_VERSION) && R_VERSION >= R_Version(3, 5, 0)
static R_xlen_t sum_isna_int_region(SEXP x) {
  if (INTEGER_NO_NA(x)) {
    return 0;
  }
  R_xlen_t N = xlength(x);
  R_xlen_t o = 0;
  int buf[SUM_ISNA_REGION_SIZE];
  for (R_xlen_t i = 0; i < N; ) {
    R_xlen_t want = N - i;
    if (want > SUM_ISNA_REGION_SIZE) {
      want = SUM_ISNA_REGION_SIZE;
    }
    R_xlen_t got = INTEGER_GET_REGION(x, i, want, buf);
    if (got <= 0) {
      error("Internal error(sum_isna): INTEGER_GET_REGION returned no data."); // # nocov
    }
    for (R_xlen_t j = 0; j < got; ++j) {
      o += buf[j] == NA_INTEGER;
    }
    i += got;
  }
  return o;
}
#endif

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

#if defined(R_VERSION) && R_VERSION >= R_Version(3, 5, 0)
static R_xlen_t sum_isna_dbl_region(SEXP x) {
  if (REAL_NO_NA(x)) {
    return 0;
  }
  R_xlen_t N = xlength(x);
  R_xlen_t o = 0;
  double buf[SUM_ISNA_REGION_SIZE];
  for (R_xlen_t i = 0; i < N; ) {
    R_xlen_t want = N - i;
    if (want > SUM_ISNA_REGION_SIZE) {
      want = SUM_ISNA_REGION_SIZE;
    }
    R_xlen_t got = REAL_GET_REGION(x, i, want, buf);
    if (got <= 0) {
      error("Internal error(sum_isna): REAL_GET_REGION returned no data."); // # nocov
    }
    for (R_xlen_t j = 0; j < got; ++j) {
      o += ISNAN(buf[j]);
    }
    i += got;
  }
  return o;
}
#endif

#if defined(R_VERSION) && R_VERSION >= R_Version(3, 6, 0)
static R_xlen_t sum_isna_lgl_region(SEXP x) {
  if (LOGICAL_NO_NA(x)) {
    return 0;
  }
  R_xlen_t N = xlength(x);
  R_xlen_t o = 0;
  int buf[SUM_ISNA_REGION_SIZE];
  for (R_xlen_t i = 0; i < N; ) {
    R_xlen_t want = N - i;
    if (want > SUM_ISNA_REGION_SIZE) {
      want = SUM_ISNA_REGION_SIZE;
    }
    R_xlen_t got = LOGICAL_GET_REGION(x, i, want, buf);
    if (got <= 0) {
      error("Internal error(sum_isna): LOGICAL_GET_REGION returned no data."); // # nocov
    }
    for (R_xlen_t j = 0; j < got; ++j) {
      o += buf[j] == NA_INTEGER;
    }
    i += got;
  }
  return o;
}
#endif

R_xlen_t sum_isna_chr(SEXP x) {
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 5, 0)
  if (STRING_NO_NA(x)) {
    return 0;
  }
#endif
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

#if defined(R_VERSION) && R_VERSION >= R_Version(3, 6, 0)
static R_xlen_t sum_isna_complx_region(SEXP x) {
  R_xlen_t N = xlength(x);
  R_xlen_t o = 0;
  Rcomplex buf[SUM_ISNA_REGION_SIZE];
  for (R_xlen_t i = 0; i < N; ) {
    R_xlen_t want = N - i;
    if (want > SUM_ISNA_REGION_SIZE) {
      want = SUM_ISNA_REGION_SIZE;
    }
    R_xlen_t got = COMPLEX_GET_REGION(x, i, want, buf);
    if (got <= 0) {
      error("Internal error(sum_isna): COMPLEX_GET_REGION returned no data."); // # nocov
    }
    for (R_xlen_t j = 0; j < got; ++j) {
      o += ISNAN(buf[j].r) || ISNAN(buf[j].i);
    }
    i += got;
  }
  return o;
}
#endif

R_xlen_t sum_isna(SEXP x, SEXP nthreads) {
  R_xlen_t N = xlength(x);
  int nThread = as_nThread(nthreads);
  switch(TYPEOF(x)) {
  case LGLSXP:
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 6, 0)
    if (is_altrep(x)) {
      return sum_isna_lgl_region(x);
    }
#endif
    return sum_isna_int(LOGICAL(x), N, nThread);
  case INTSXP:
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 5, 0)
    if (is_altrep(x)) {
      return sum_isna_int_region(x);
    }
#endif
    return sum_isna_int(INTEGER(x), N, nThread);
  case REALSXP:
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 5, 0)
    if (is_altrep(x)) {
      return sum_isna_dbl_region(x);
    }
#endif
    return sum_isna_dbl(REAL(x), N, nThread);
  case STRSXP:
    return sum_isna_chr(x);
  case CPLXSXP:
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 6, 0)
    if (is_altrep(x)) {
      return sum_isna_complx_region(x);
    }
#endif
    return sum_isna_complx(x);
  }
  return 0;
}

SEXP Csum_isna(SEXP x, SEXP nthreads) {
  return ScalarLength(sum_isna(x, nthreads));
}


