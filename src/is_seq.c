#include "hutilscpp.h"

bool is_seq(SEXP x) {
  switch(TYPEOF(x)) {
  case INTSXP:
    if (is_altrep(x)) {
      return true;
    } else {

      const int * xp = INTEGER(x);
      int x0 = xp[0];
      R_xlen_t N = xlength(x);
      for (R_xlen_t i = 1; i < N; ++i) {
        if (xp[i] != x0 + i) {
          return false;
        }
      }
      return true;
    }
  case REALSXP:
    if (is_altrep(x)) {
      return true;
    }
  }
  return false;
}

SEXP Cis_seq(SEXP x) {
  return ScalarLogical(is_seq(x));
}

