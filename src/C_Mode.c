#include "hutilscpp.h"

SEXP C_Mode(SEXP x) {
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
#pragma omp parallel for reduction(min : xmin) reduction(max : xmax)
  for (R_xlen_t i = 1; i < N; ++i) {
    int xpi = xp[i];
    if (xpi < xmin) {
      xmin = xpi;
    } else if (xpi > xmax) {
      xmax = xpi;
    }
  }
  unsigned int range = xmax + 1u;
  range -= xmin;
  unsigned int * tbl = malloc(sizeof(int) * range);
  if (tbl == NULL) {
    free(tbl);
    error("tbl count not be malloc'd."); // # nocov
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

