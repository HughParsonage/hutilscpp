#include "hutilscpp.h"

#define F_AVG 1
#define F_MIN 2
#define F_MAX 3

int fun2int(const char * x) {
  char x0 = x[0];
  if (x0 == '\0' || x[1] == '\0' || x[2] == '\0') {
    return -1; // # nocov
  }
  char x1 = x[1];
  char x2 = x[2];
  if (x0 == 'm' &&
      x1 == 'a' &&
      x2 == 'x') {
    return F_MAX;
  }
  if (x0 == 'm' &&
      x1 == 'i' &&
      x2 == 'n') {
    return F_MIN;
  }
  return -1; // # nocov
}

SEXP Csummary3(SEXP xx, SEXP yy, SEXP zz,
               SEXP Fun,
               SEXP nthreads) {
  if (TYPEOF(xx) != TYPEOF(yy) ||
      TYPEOF(xx) != TYPEOF(zz) ||
      TYPEOF(yy) != TYPEOF(zz) ||
      TYPEOF(nthreads) != INTSXP ||
      xlength(nthreads) != 1) {
    return R_NilValue; // # nocov
  }
  if (TYPEOF(xx) != REALSXP && TYPEOF(xx) != INTSXP) {
    return R_NilValue;
  }

  const int fun = fun2int(CHAR(STRING_ELT(Fun, 0)));

  const R_xlen_t N = xlength(xx);
  const R_xlen_t ny = xlength(yy);
  const R_xlen_t nz = xlength(zz);
  // # nocov start
  if (N == 0 || ny == 0 || nz == 0) {
    return allocVector(TYPEOF(xx), 0);
  }
  if (ny != N && ny != 1) {
    return R_NilValue;
  }
  if (nz != N && nz != 1) {
    return R_NilValue;
  }
  // # nocov end

  const bool y1 = ny == 1;
  const bool z1 = nz == 1;
  int nThread = asInteger(nthreads);

  switch(TYPEOF(xx)) {
  case INTSXP: {
    const int * x = INTEGER(xx);
    const int * y = INTEGER(yy);
    const int * z = INTEGER(zz);
    const int y0 = y[0];
    const int z0 = z[0];
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * out = INTEGER(ans);
    switch(fun) {
    case F_MAX:
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = x[i];
        int yi = y1 ? y0 : y[i];
        int zi = z1 ? z0 : z[i];
        out[i] = maxi3(xi, yi, zi);
      }
      break;
    case F_MIN:
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = x[i];
        int yi = y1 ? y0 : y[i];
        int zi = z1 ? z0 : z[i];
        out[i] = mini3(xi, yi, zi);
      }
      break;
    }
    UNPROTECT(1);
    return ans;
  }
    break;
  case REALSXP: {
    const double * x = REAL(xx);
    const double * y = REAL(yy);
    const double * z = REAL(zz);
    const double y0 = y[0];
    const double z0 = z[0];
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * out = REAL(ans);
    switch(fun) {
    case F_MAX:
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        double xi = x[i];
        double yi = y1 ? y0 : y[i];
        double zi = z1 ? z0 : z[i];
        out[i] = maxd3(xi, yi, zi);
      }
      break;
    case F_MIN:
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        double xi = x[i];
        double yi = y1 ? y0 : y[i];
        double zi = z1 ? z0 : z[i];
        out[i] = mind3(xi, yi, zi);
      }
    }
    UNPROTECT(1);
    return ans;
  }
  }

  return R_NilValue; // # nocov
}
