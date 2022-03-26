#include "hutilscpp.h"



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
  const int * tp = LOGICAL(table);
  for (int j = 0; j < M; ++j) {
    if (tbl[0] && tbl[1] && tbl[2]) {
      break;
    }
    int xpj = xp[j];
    if (xpj == 0 && !tbl[0]) {
      tbl[0] = j + 1;
      continue;
    }
    if (xpj == 1 && !tbl[1]) {
      tbl[1] = j + 1;
      continue;
    }
    if (!tbl[2]) {
      tbl[2] = j + 1;
    }

  }
  if (fin) {
    SEXP ans = PROTECT(allocVector(LGLSXP, N));
    int * ansp = LOGICAL(ans);
    if (tbl[0] && tbl[1]) {
      FORLOOP(ansp[i] = 1;)
    } else if (tbl[0] && tbl[2]) {
      FORLOOP(ansp[i] = xp[i] != 1;)
    } else if (tbl[1] && tbl[2]) {
      FORLOOP(ansp[i] = xp[i] != 0;)
    } else if (tbl[0]) {
      FORLOOP(ansp[i] = xp[i] == 0;)
    } else {
      FORLOOP(ansp[i] = xp[i] == 1;)
    }
    UNPROTECT(1);
    return ans;
  } else {
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * ansp = INTEGER(ans);
    FORLOOP(ansp[i] = xp[i] == NA_LOGICAL ? tbl[2] : (xp[i] ? tbl[1] : tbl[0]);)
    UNPROTECT(1);
    return ans;
  }

}
