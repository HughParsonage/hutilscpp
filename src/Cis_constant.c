#include "hutilscpp.h"

bool is_constant_int(const int * x, R_xlen_t N, int nThread) {
  const int x0 = x[0];
  char o = true;

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(& : o) schedule(static)
#endif
  for (R_xlen_t i = 1; i < N; ++i) {
    o &= x[i] == x0;
  }
  return o;
}

bool is_constant_dbl(const double * x, R_xlen_t N, int nThread) {
  const double x0 = x[0];
  char o = true;
  if (ISNAN(x0)) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(& : o) schedule(static)
#endif
    for (R_xlen_t i = 1; i < N; ++i) {
      o &= ISNAN(x[i]);
    }
    return o;
  }

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(& : o) schedule(static)
#endif
  for (R_xlen_t i = 1; i < N; ++i) {
    o &= x[i] == x0;
  }
  return o;
}

bool is_constant_chr(SEXP x) {
  R_xlen_t N = xlength(x);
  const char * x0 = CHAR(STRING_ELT(x, 0));
  int n0 = strlen(x0);

  for (R_xlen_t i = 1; i < N; ++i) {
    const char * xi = CHAR(STRING_ELT(x, i));
    if (!string_equaln(x0, n0, xi)) {
      return false;
    }
  }
  return true;
}

bool is_constant_complex(SEXP x) {
  R_xlen_t N = xlength(x);
  Rcomplex x0 = COMPLEX_ELT(x, 0);
  double r0 = x0.r;
  double i0 = x0.i;
  for (R_xlen_t i = 1; i < N; ++i) {
    Rcomplex xi = COMPLEX_ELT(x, i);
    double ri = xi.r;
    double ii = xi.i;
    if (ri != r0 || ii != i0) {
      return false;
    }
  }
  return true;
}

bool is_constant_raw(const Rbyte * xp, R_xlen_t N, int nThread) {
  const Rbyte x0 = xp[0];
  for (R_xlen_t i = 1; i < N; ++i) {
    if (xp[i] != x0) {
      return false;
    }
  }
  return true;
}

SEXP Cis_constant(SEXP x, SEXP nthreads) {
  R_xlen_t N = xlength(x);
  if (N <= 1) {
    return ScalarLogical(1);
  }
  int nThread = asInteger(nthreads);
  switch(TYPEOF(x)) {
  case LGLSXP:
  case INTSXP:
    return ScalarLogical(is_constant_int(INTEGER(x), N, nThread));
  case REALSXP:
    return ScalarLogical(is_constant_dbl(REAL(x), N, nThread));
  case STRSXP:
    return ScalarLogical(is_constant_chr(x));
  case CPLXSXP:
    return ScalarLogical(is_constant_complex(x));
  case RAWSXP:
    return ScalarLogical(is_constant_raw(RAW(x), N, nThread));
  }
  return R_NilValue; // # nocov
}

R_xlen_t isntConstant_int(const int * x, R_xlen_t N) {
  int x0 = x[0];
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i] != x0) {
      return i + 1;
    }
  }
  return 0;
}

R_xlen_t isntConstant_dbl(const double * x, R_xlen_t N) {
  double x0 = x[0];
  if (ISNAN(x0)) {
    for (R_xlen_t i = 1; i < N; ++i) {
      if (!ISNAN(x[i])) {
        return i + 1;
      }
    }
    return 0;
  }
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i] != x0) {
      return i + 1;
    }
  }
  return 0;
}

R_xlen_t isntConstant_chr(SEXP x) {
  R_xlen_t N = xlength(x);
  if (STRING_ELT(x, 0) == NA_STRING) {
    for (R_xlen_t i = 1; i < N; ++i) {
      if (STRING_ELT(x, i) != NA_STRING) {
        return i + 1;
      }
    }
    return 0;
  }
  const char * x0 = CHAR(STRING_ELT(x, 0));
  int n0 = strlen(x0);
  for (R_xlen_t i = 1; i < N; ++i) {
    const char * xi = CHAR(STRING_ELT(x, i));
    if (!string_equaln(x0, n0, xi)) {
      return i + 1;
    }
  }
  return 0;
}

R_xlen_t isntConstant_complex(SEXP x) {
  R_xlen_t N = xlength(x);
  const Rcomplex x0 = COMPLEX_ELT(x, 0);
  const double r0 = x0.r;
  const double i0 = x0.i;
  for (R_xlen_t i = 1; i < N; ++i) {
    Rcomplex xi = COMPLEX_ELT(x, i);
    double ri = xi.r;
    double ii = xi.i;
    if (ri != r0 || ii != i0) {
      return i + 1;
    }
  }
  return 0;
}

R_xlen_t isntConstant_raw(const Rbyte * xp, R_xlen_t N) {
  const Rbyte x0 = xp[0];
  for (R_xlen_t i = 1; i < N; ++i) {
    if (xp[i] != x0) {
      return i + 1;
    }
  }
  return 0;
}

SEXP Cisnt_constant(SEXP x) {
  R_xlen_t N = xlength(x);
  if (N <= 1) {
    return ScalarInteger(0); // # nocov
  }
  switch(TYPEOF(x)) {
  case LGLSXP:
  case INTSXP:
    return ScalarLength(isntConstant_int(INTEGER(x), N));
  case REALSXP:
    return ScalarLength(isntConstant_dbl(REAL(x), N));
  case STRSXP:
    return ScalarLength(isntConstant_chr(x));
  case CPLXSXP:
      return ScalarLength(isntConstant_complex(x));
  case RAWSXP:
    return ScalarLength(isntConstant_raw(RAW(x), N));
  }
  return R_NilValue; // # nocov
}


