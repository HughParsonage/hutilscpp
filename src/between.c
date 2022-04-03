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

// given double y0 and y1, convert to integer for preparation
void bw_y0_y1(int y0y1[2], double y0, double y1) {
  if (ISNAN(y0) || ISNAN(y1)) {
    if (ISNAN(y0)) {
      y0y1[0] = INT_MIN;
    }
    if (ISNAN(y1)) {
      y0y1[1] = INT_MAX;
    }
  } else {
    if (y0 == y1) {
      if (do_is_safe2int(y0)) {
        int y0i = y0;
        y0y1[0] = y0i;
        y0y1[1] = y0i;
      } else {
        y0y1[0] = 1; // just any x > y
        y0y1[1] = 0;
      }
      return;
    }
    if (y0 > y1) {
      y0y1[0] = 1;
      y0y1[1] = 0;
      return;
    }
  }
  switch(why_dbl_isnt_int(y0)) {
  case DBL_INT:
    y0y1[0] = y0;
    break;
  case DBL_FRA:
    y0y1[0] = y0 + (y0 > 0);
    break;
  case DBL_XLO:
    y0y1[0] = INT_MIN;
    break;
  case DBL_XHI:
    y0y1[0] = INT_MAX, y0y1[1] = 0;
    return;
    // don't need to consider y1y1[1]
    //
  case DBL_NAN:
    y0y1[0] = INT_MIN; // # nocov
    break;
  }
  switch(why_dbl_isnt_int(y1)) {
  case DBL_INT:
    y0y1[1] = y1;
    break;
  case DBL_FRA:
    y0y1[1] = y1 + (y1 < 0);
    break;
  case DBL_XLO:
    y0y1[0] = 1, y0y1[1] = 0;
    break;
  case DBL_XHI:
    y0y1[1] = INT_MAX;
    break;
  }
}

void uc_betweenidd(unsigned char * ansp,
                   int ORAND, /* int for OR AND EQUAL */
                   const int * xp,
                   R_xlen_t N,
                   int nThread,
                   double y0,
                   double y1) {
  int y0y1[2] = {0};
  bw_y0_y1(y0y1, y0, y1);
  const int y0i = y0y1[0];
  const int y1i = y0y1[1];
  if (y0i > y1i) {
    if (ORAND == ORAND_OR) {
      return;
    }
    FORLOOP(ansp[i] = 0;)
    return;
  }
  if (y0i == y1i) {
    FORLOOP(ansp[i] = xp[i] == y0i;)
    return;
  }
  switch(ORAND) {
  case ORAND_OR:
    FORLOOP(ansp[i] |= betweenii64(xp[i], y0i, y1i);)
    break;
  case ORAND_AND:
    FORLOOP(ansp[i] &= betweenii64(xp[i], y0i, y1i);)
    break;
    // # nocov start
  case ORAND_EQ:
    FORLOOP(ansp[i] = betweenii64(xp[i], y0i, y1i);)
    break;
    // # nocov end
  }
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
    return R_NilValue; // # nocov
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
  return R_NilValue; // # nocov
}
