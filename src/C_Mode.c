#include "hutilscpp.h"

SEXP C_Mode(SEXP x, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  if (!isInteger(x)) {
    error("not integer.");
  }
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  if (N <= 256) {
    unsigned int tbl[256] = {0};
    bool dup[256] = {0};
    tbl[0] = 1;
    for (R_xlen_t i = 0; i < N; ++i) {
      if (dup[i]) {
        continue;
      }
      int xpi = xp[i];
      for (R_xlen_t j = i + 1; j < N; ++j) {
        int xpj = xp[j];
        if (xpj == xpi) {
          dup[j] = true;
          tbl[i]++;
        }
      }
    }
    int ans = 0;
    int tbl_max = 1;
    for (int k = 0; k < 256; ++k) {
      if (tbl[k] > tbl_max) {
        ans = k;
        tbl_max = tbl[k];
      }
    }
    return ScalarInteger(xp[ans]);
  }

  int xmin = xp[0];
  int xmax = xp[0];

  FORLOOP_xminmax({
    int xpi = xp[i];
    if (xpi < xmin) {
      xmin = xpi;
    } else if (xpi > xmax) {
      xmax = xpi;
    }
  };)

  R_xlen_t xrange_t = (R_xlen_t)xmax - (R_xlen_t)xmin;
  unsigned int range = xmax + 1u;
  range += ((unsigned int)(-xmin));
  if ((range >> 2) > N) {
    return R_NilValue;
  }

  unsigned int * tbl = malloc(sizeof(int) * range);
  if (tbl == NULL) {
    free(tbl);
    error("tbl could not be malloc'd."); // # nocov
  }
  memset(tbl, 0, sizeof(int) * range);

  for (R_xlen_t i = 0; i < N; ++i) {
    int64_t xpi = xp[i];
    int64_t di = xpi - xmin;
    tbl[di]++;
  }

  int tmax = 1;
  unsigned int ans = 0;
  for (unsigned int t = 0; t < range; ++t) {
    if (tbl[t] > tmax) {
      tmax = tbl[t];
      ans = t;
    }
  }
  free(tbl);
  return ScalarInteger((int)ans + xmin);
}

SEXP Cunique_fmatch(SEXP xx, SEXP ff, SEXP nthreads) {
  if (!isInteger(xx) || !isInteger(ff) || xlength(xx) <= 1) {
    return xx;
  }
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(xx);
  const int * f = INTEGER(ff);
  const int * x = INTEGER(xx);
  int o = 0, M = 0;
  FORLOOP_redsum(o += f[i] >= (i + 1);)

  M = o;

  SEXP ans = PROTECT(allocVector(INTSXP, M));
  int * ansp = INTEGER(ans);
  ansp[0] = x[0]; // by definition
  R_xlen_t j = 0;
  for (R_xlen_t i = 1; i < N; ++i) {
    if (f[i] >= (i + 1)) {
      ++j;
      ansp[j] = x[i];
    }
  }
  UNPROTECT(1);
  return ans;
}

SEXP CuniqueN_fmatch(SEXP fx, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(fx);
  if (!isInteger(fx)) {
    error("Expected fx to be integer."); // # nocov
  }
  const int * f = INTEGER(fx);
  int o = 0;
  FORLOOP_redsum(o += f[i] > i;)

  return ScalarInteger(o);
}



