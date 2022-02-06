#include "hutilscpp.h"

SEXP Cuminus(SEXP x, SEXP y) {
  unsigned int xx = asInteger(x);
  unsigned int yy = asInteger(y);
  unsigned int u = xx - yy;
  Rprintf("%u\n", u);
  return ScalarInteger(u);
}
