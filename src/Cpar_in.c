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
                    bool opposite) {
  int ymin = yp[0], ymax = yp[0];
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(min : ymin) reduction(max : ymax)
#endif
  for (int i = 1; i < M; ++i) {
    int ypi = yp[i];
    ymin = ypi < ymin ? ypi : ymin;
    ymax = ypi > ymax ? ypi : ymax;
  }
  int64_t y_range = ymax;
  y_range -= ymin;
  y_range++;
  unsigned char * yc = malloc(sizeof(char) * y_range);
  if (yc == NULL) {
    fail[0] = 1;
    return;
  }
  memset(yc, 0, sizeof(char) * y_range);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (int i = 0; i < M; ++i) {
    yc[yp[i] - ymin] = 1;
  }
  memset(ansp, !opposite, sizeof(char) * N);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    unsigned int upi = xpi - ymin;
    if (upi >= y_range) {
      continue;
    }
    ansp[i] ^= yc[upi];
  }
  free(yc);
  fail[0] = 0;

}

SEXP Cpar_in_intchar(SEXP xx, SEXP yy, SEXP nthreads) {

  R_xlen_t N = xlength(xx);
  if (TYPEOF(xx) != INTSXP) {
    error("Internal error(Cpar_in_int): TYPEOF(xx) != INTSXP."); // # nocov
  }
  if (xlength(yy) > INT_MAX) {
    error("xlength(yy) > INT_MAX"); // # nocov
  }
  int M = xlength(yy);
  if (M == 0) {
    return RawN(N);
  }
  if (is_seq(yy)) {

  }

  int nThread = as_nThread(nthreads);
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * ansp = RAW(ans);
  const int * xp = INTEGER(xx);
  const int * yp = INTEGER(yy);


  unsigned int fail[1] = {0};
  do_uchar_in_II(ansp, fail, xp, N, yp, M, nThread, true);
  if (fail[0]) {
    UNPROTECT(1);
    return R_NilValue;
  }
  UNPROTECT(1);
  return ans;
}
