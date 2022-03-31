#include "hutilscpp.h"

static void lgl_flip(int * x, R_xlen_t N) {
  for (R_xlen_t i = 0; i < N; ++i) {
    if (x[i]) {
      x[i] = 0;
    } else {
      x[i] = 1;
    }
  }
}

static void raw_flip(unsigned char * x, R_xlen_t N) {
  for (R_xlen_t i = 0; i < N; ++i) {
    x[i] ^= 1;
  }
}

SEXP C_FLIP(SEXP x) {
  switch(TYPEOF(x)) {
  case LGLSXP:
    lgl_flip(LOGICAL(x), xlength(x));
    break;
  case RAWSXP:
    raw_flip(RAW(x), xlength(x));
  }
  return x;
}
