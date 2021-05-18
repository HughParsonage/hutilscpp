#include "hutilscpp.h"

bool string_equal(const char * x, const char * y) {
  if (x[0] == '\0') {
    return y[0] == '\0';
  }
  int i = 0;
  while (x[i] != '\0' && y[i] != '\0') {
    if (x[i] != y[i]) {
      return false;
    }
    ++i;
  }
  return x[i] == y[i];
}

bool string_equaln(const char * x, int nx, const char * y) {
  if (nx == 0) {
    return y[0] == '\0';
  }
  for (int i = 0; i < nx; ++i) {
    if (y[i] == '\0' || x[i] != y[i]) {
      return false;
    }
  }
  return true;
}

SEXP CStringEqual(SEXP x, SEXP y) {
  if (TYPEOF(x) != STRSXP || TYPEOF(y) != STRSXP) {
    return ScalarLogical(0);
  }
  if (xlength(y) == 1) {
    const char * y0 = CHAR(STRING_ELT(y, 0));
    int ny = strlen(y0);
    Rprintf("ny = %d\n", ny);
    R_xlen_t N = xlength(x);
    for (R_xlen_t i = 0; i < N; ++i) {
      const char * xi = CHAR(STRING_ELT(x, i));
      if (!string_equaln(y0, ny, xi)) {
        return ScalarLogical(0);
      }
    }
    return ScalarLogical(1);
  }
  if (TYPEOF(x) != STRSXP || TYPEOF(y) != STRSXP ||
      xlength(x) != xlength(y)) {
    return ScalarLogical(0);
  }
  R_xlen_t N = xlength(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    const char * xi = CHAR(STRING_ELT(x, i));
    const char * yi = CHAR(STRING_ELT(y, i));
    if (!string_equal(xi, yi)) {
      return ScalarLogical(0);
    }
  }
  return ScalarLogical(1);
}
