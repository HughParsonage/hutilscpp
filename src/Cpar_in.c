#include "hutilscpp.h"

SEXP Cpar_in_int(SEXP xx, SEXP yy, SEXP nthreads) {
  R_xlen_t N = xlength(xx);
  if (TYPEOF(xx) != INTSXP) {
    error("Internal error(Cpar_in_int): TYPEOF(xx) != INTSXP."); // # nocov
  }
  if (xlength(yy) > INT_MAX) {
    error("xlength(yy) > INT_MAX"); // # nocov
  }
  int tn = xlength(yy);
  const int * xp = INTEGER(xx);
  const int * yp = INTEGER(yy);
  int nThread = as_nThread(nthreads);
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    int oi = 0;
    int xi = xp[i];
    for (int j = 0; j < tn; ++j) {
      if (yp[j] == xi) {
        oi = 1;
        break;
      }
    }
    ansp[i] = oi;
  }
  UNPROTECT(1);
  return ans;

}

void do_uchar_in_II(unsigned char * ansp,
                    unsigned int * fail,
                    const int * xp,
                    R_xlen_t N,
                    const int * yp,
                    R_xlen_t M,
                    int nThread,
                    bool opposite,
                    int yminmax[2]) {
  int ymin = yminmax[0], ymax = yminmax[1];
  if (ymax < ymin) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(min : ymin) reduction(max : ymax)
#endif
    for (int i = 1; i < M; ++i) {
      int ypi = yp[i];
      ymin = ypi < ymin ? ypi : ymin;
      ymax = ypi > ymax ? ypi : ymax;
    }
  }
  if (ymin >= 0) {
    // can assume range is ok and differences are integer, always
    int y_range = ymax + 1 - ymin;
    unsigned char * yc = calloc(y_range, sizeof(char));
    if (yc == NULL) {
      fail[0] = 1; // # nocov
      return; // # nocov
    }
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < M; ++i) {
      yc[yp[i] - ymin] = !opposite;
    }
    memset(ansp, 0, sizeof(char) * N);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < N; ++i) {
      unsigned int xpi = xp[i];
      unsigned int upi = xpi - ymin;
      if (upi >= y_range) {
        continue;
      }
      ansp[i] = yc[upi];
    }
    free(yc);
    fail[0] = 0;
    return;
  }
  // Otherwise we have to be more careful
  int64_t y_range = ymax;
  int64_t ymin64 = ymin;
  y_range -= ymin64;
  y_range++;
  unsigned char * yc = malloc(sizeof(char) * y_range);
  if (yc == NULL) {
    fail[0] = 1; // # nocov
    return; // # nocov
  }
  memset(yc, 0, sizeof(char) * y_range);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (int i = 0; i < M; ++i) {
    int64_t ypi = yp[i];
    yc[ypi - ymin64] = 1;
  }

  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = 0;
    int xpi = xp[i];
    if (xpi < ymin || xpi > ymax) {
      continue;
    }
    int64_t upi = (int64_t)xpi - ymin64;
    ansp[i] = yc[upi];
  }
  free(yc);
  fail[0] = 0;

}

SEXP par_in_intchar(SEXP xx, SEXP yy, int nThread, int yminmax[2], bool opposite) {
  const int * xp = INTEGER(xx);
  const int * yp = INTEGER(yy);
  R_xlen_t N = xlength(xx);
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * ansp = RAW(ans);
  unsigned int fail[2] = {0};
  do_uchar_in_II(ansp, fail, xp, N, yp, xlength(yy), nThread, opposite, yminmax);
  UNPROTECT(1);
  return ans;
}


