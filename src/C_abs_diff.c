#include "hutilscpp.h"

int64_t single_abs_diff(int x, int y) {
  int64_t x64 = x;
  int64_t y64 = y;
  int64_t d = y64 - x64;
  return d >= 0 ? d : -d;
}

static double dsingle_abs_diff(double x, double y) {
  return x > y ? x - y : y - x;
}


int64_t max_abs_diffii(const int * x, const int * y, R_xlen_t N, R_xlen_t M, int nThread) {
  if (N != M && M != 1) {
    return 0; // but should be erroneous // # nocov
  }
  int64_t y0 = y[0];
  int64_t x0 = x[0];
  int64_t dmax = (x0 > y0) ? x0 - y0 : y0 - x0;
  if (N != M && M == 1) {

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(max : dmax)
#endif
    for (R_xlen_t i = 1; i < N; ++i) {
      int64_t xi = x[i];

      if (xi > y0) {
        int64_t di = xi - y0;
        if (di > dmax) {
          dmax = di;
        }
      } else {
        int64_t di = y0 - xi;
        if (di > dmax) {
          dmax = di;
        }
      }
    }
  } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(max : dmax)
#endif
    for (R_xlen_t i = 1; i < N; ++i) {
      int64_t xi = x[i];
      int64_t y0 = y[i];
      if (xi > y0) {
        int64_t di = xi - y0;
        if (di > dmax) {
          dmax = di;
        }
      } else {
        int64_t di = y0 - xi;
        if (di > dmax) {
          dmax = di;
        }
      }
    }
  }

  return dmax;
}

SEXP abs_dbl_dbl(SEXP x, SEXP y, SEXP nthreads, SEXP Option) {
  if (xlength(x) == 0 || xlength(y) == 0) {
    return R_NilValue; // # nocov
  }
  // # nocov start
  if (!isReal(x) || !isReal(y)) {
    error("Internal error(abs_dbl_dbl): x was type '%s' and y was type '%s' but REALSXP were expected",
          type2char(TYPEOF(x)), type2char(TYPEOF(y)));
  }
  // # nocov end
  const int opt = asInteger(Option);
  int nThread = asInteger(nthreads);
  R_xlen_t N = xlength(x);
  const double * xp = REAL(x);
  const double * yp = REAL(y);
  const double y0 = asReal(y);
  if (xlength(y) == 1) {
    if (opt == 0) {
      double ans = dsingle_abs_diff(xp[0], y0);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(max : ans)
#endif
      for (R_xlen_t i = 1; i < N; ++i) {
        ans = maxdd(ans, dsingle_abs_diff(xp[i], y0));
      }
      return ScalarReal(ans);
    }
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * restrict ansp = REAL(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = dsingle_abs_diff(xp[i], y0);
    }
    UNPROTECT(1);
    return ans;
  }
  if (opt == 0) {
    double ans = dsingle_abs_diff(xp[0], y0);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(max : ans)
#endif
    for (R_xlen_t i = 1; i < N; ++i) {
      ans = maxdd(ans, dsingle_abs_diff(xp[i], yp[i]));
    }
    return ScalarReal(ans);
  }
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = dsingle_abs_diff(xp[i], yp[i]);
  }
  UNPROTECT(1);
  return ans;
}

SEXP C_abs_diff(SEXP x, SEXP y, SEXP nthreads, SEXP Option) {
  if (xlength(x) != xlength(y) && xlength(y) != 1) {
    return R_NilValue; // # nocov
  }
  if (isReal(x) && isReal(y)) {
    return abs_dbl_dbl(x, y, nthreads, Option);
  }
  const int opt = asInteger2(Option);
  // # nocov start
  if (!isInteger(x) || !isInteger(y) || xlength(x) == 0 ||
      opt < 0 || opt > 2) {
    return R_NilValue;
  }
  // # nocov end
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  const int * yp = INTEGER(y);

  // Determine whether doubles (64-bit int) are required
  bool needs64 = true;
  // If opt == 2 then we always use double so no need
  // ... to determine need
  if (opt != 2) {
    // detect whether the result will be integer
    int64_t max_abs_xy = max_abs_diffii(xp, yp, N, xlength(y), nThread);
    if (opt == 0) {
      return ScalarLength(max_abs_xy);
    }
    needs64 = max_abs_xy >= INT_MAX;
  }
  if (needs64) {
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * restrict ansp = REAL(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = single_abs_diff(xp[i], yp[i]);
    }
    UNPROTECT(1);
    return ans;
  }

  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = single_abs_diff(xp[i], yp[i]);
  }
  UNPROTECT(1);
  return ans;


}
