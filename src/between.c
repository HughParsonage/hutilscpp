#include "hutilscpp.h"

bool betweeniiuu(unsigned int x, unsigned int a, unsigned b) {
  unsigned int b_minus_a = b - a;
  return x - a <= b_minus_a;
}

bool betweenii64(int x, int a, int b) {
  int64_t a64 = a;
  int64_t b64 = b;
  unsigned int b_minus_a = b64 - a64;
  return (unsigned int)x - (unsigned int)a <= b_minus_a;
}

bool betweenii64_bma(unsigned int x, unsigned int a, unsigned int b_minus_a) {
  return x - a <= b_minus_a;
}

bool betweenii_asis(int x, int a, int b) {
  return x >= a && x <= b;
}

SEXP BetweenIii(SEXP x, int a, int b, int m, int nThread) {
  R_xlen_t N = xlength(x);
  if (a > b) {
    return RawN(N);
  }
  const int * xp = INTEGER(x);
  int64_t b64 = b;
  int64_t a64 = a;
  unsigned int b_minus_a = b64 - a64;
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * ansp = RAW(ans);

  switch(m) {
  case 0:
    FORLOOP(ansp[i] = betweenii64(xp[i], a, b);)
    break;
  case 1:
    FORLOOP(ansp[i] = betweeniiuu(xp[i], a, b);)
    break;
  case 2:
    FORLOOP(ansp[i] = betweenii64_bma(xp[i], a, b_minus_a);)
    break;
  case 3:
    FORLOOP(ansp[i] = betweenii_asis(xp[i], a, b);)
    break;
  }
  UNPROTECT(1);
  return ans;
}

SEXP CBetween(SEXP x, SEXP a, SEXP b, SEXP m, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  if (!isInteger(m)) {
    return R_NilValue;
  }
  if (xlength(a) == 1) {
    switch(TYPEOF(x)) {
    case INTSXP:
      switch(TYPEOF(a)) {
      case INTSXP:
        return BetweenIii(x, asInteger(a), asInteger(b), asInteger(m), nThread);
      }
    }
  }
  return R_NilValue;
}
