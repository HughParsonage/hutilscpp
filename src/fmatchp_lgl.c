#include "hutilscpp.h"

unsigned int encode_lgl(int x) {
  return x == NA_LOGICAL ? 2 : x;
}

SEXP fmatchp_lgl(SEXP x, SEXP table, SEXP nthreads, SEXP Fin) {
  if (!isLogical(x)) {
    error("Internal error: x not type LGLSXP."); // # nocov
  }
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(x);
  const int * xp = LOGICAL(x);
  const bool fin = asLogical(Fin);
  int tbl[3] = {0};
  int M = length(table);
  if (M == 0) {
    return LogicalN(N);
  }
  if (!isLogical(table)) {
    error("`table` was type '%s' but must be logical.", type2char(TYPEOF(table))); // # nocov
  }
  const int * tp = LOGICAL(table);
  for (int j = 0; j < M; ++j) {
    if (tbl[0] && tbl[1] && tbl[2]) {
      break;
    }
    int xpj = tp[j];
    int epj = encode_lgl(xpj);
    if (tbl[epj]) {
      continue;
    }
    tbl[epj] = j + 1;
  }
  if (fin) {
    SEXP ans = PROTECT(allocVector(LGLSXP, N));
    int * ansp = LOGICAL(ans);
    if (tbl[0] && tbl[1]) {
      FORLOOP(ansp[i] = 1;)
    } else {
      FORLOOP({
        ansp[i] = (bool)tbl[encode_lgl(xp[i])];
      })
    }
    UNPROTECT(1);
    return ans;
  } else {
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * ansp = INTEGER(ans);
    FORLOOP(ansp[i] = tbl[encode_lgl(xp[i])];)
    UNPROTECT(1);
    return ans;
  }

}
