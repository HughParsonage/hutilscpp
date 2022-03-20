#include "hutilscpp.h"





SEXP lcoalesce0(const int * xp, R_xlen_t N, int nThread) {
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);
  FORLOOP(ansp[i] = xp[i] == NA_LOGICAL ? 0 : xp[i];);
  UNPROTECT(1);
  return ans;
}

static bool inanyNA(const int * x, R_xlen_t N, int nThread) {
  R_xlen_t J[26] =
    {64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384,
     32768, 65536, 131072, 262144, 524288, 1048576,
     2097152, 4194304, 8388608, 16777216,
     33554432, 67108864, 134217728, 268435456,
     536870912, 1073741824, 2147483648};
  if (N <= 3) {
    if (N == 0) {
      return true;
    }
    if (N == 1) {
      return x[0] != NA_INTEGER;
    }
    if (N == 2) {
      return x[0] != NA_INTEGER && x[1] != NA_INTEGER;
    }
    if (N == 3) {
      return x[0] != NA_INTEGER && x[1] != NA_INTEGER && x[2] != NA_INTEGER;
    }
  }
  if (x[0] == NA_INTEGER ||
      x[1] == NA_INTEGER ||
      x[2] == NA_INTEGER) {
    return false;
  }
  if (x[N - 3] == NA_INTEGER ||
      x[N - 2] == NA_INTEGER ||
      x[N - 1] == NA_INTEGER) {
    return false;
  }
  for (unsigned int j = 0; j < 26; ++j) {
    R_xlen_t i = J[j];
    if (i >= N - 4) {
      break;
    }

    if (x[i] == NA_INTEGER) {
      return false;
    }

  }
  bool o = true;
  FORLOOP_redand(o &= x[i] != NA_INTEGER;)
  return o;
}

SEXP icoalesce0(const int * xp, R_xlen_t N, int nThread) {
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  FORLOOP({
    ansp[i] = xp[i] == NA_INTEGER ? 0 : xp[i];
  });
  UNPROTECT(1);
  return ans;
}

SEXP dcoalesce0(const double * xp, R_xlen_t N, int nThread) {
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  FORLOOP({
    ansp[i] = ISNAN(xp[i]) ? 0 : xp[i];
  });
  UNPROTECT(1);
  return ans;
}

SEXP Ccoalesce0(SEXP x, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  switch(TYPEOF(x)) {
  case LGLSXP:
    return lcoalesce0(LOGICAL(x), xlength(x), nThread);
  case INTSXP:
    if (inanyNA(INTEGER(x), xlength(x), nThread)) {
      return x;
    } else {
      return icoalesce0(INTEGER(x), xlength(x), nThread);
    }
    break;
  case REALSXP:
    return dcoalesce0(REAL(x), xlength(x), nThread);
  case RAWSXP:
    return x;
  }
  return x;
}