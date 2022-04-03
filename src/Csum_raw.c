#include "hutilscpp.h"

SEXP Csum_raw(SEXP x, SEXP nthreads) {
  if (isntRaw(x) && !isInteger(x)) {
    return R_NilValue; // # nocov
  }
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) == INTSXP) {
    const int * xp = INTEGER(x);
    double o = 0;
    FORLOOP_redsum(o += xp[i];)
    if (o <= INT_MAX && o > NA_INT) {
      return ScalarInteger(o);
    } else {
      return ScalarReal(o);
    }

  } else {
    const unsigned char * xp = RAW(x);
    uint64_t o = 0;
    FORLOOP_redsum(o += xp[i];)
    return ScalarLength(o);
  }
  return R_NilValue; // # nocov
}
