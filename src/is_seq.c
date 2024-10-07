#include "hutilscpp.h"

bool is_seq(SEXP x) {
  if (xlength(x) == 0) {
    // since xp[0] is assumed
    return false;
  }
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
    return is_altrep(x);
  }
  return false;
}

SEXP Cis_seq(SEXP x) {
  return ScalarLogical(is_seq(x));
}

