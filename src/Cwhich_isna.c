#include "hutilscpp.h"

SEXP Cwhich_isna(SEXP x, SEXP Isnt, SEXP nthreads) {
  const bool isnt = asLogical(Isnt);
  R_xlen_t s = sum_isna(x, nthreads);
  R_xlen_t N = xlength(x);
  R_xlen_t n = isnt ? (N - s) : s;

  if (n == 0) {
    return allocVector(INTSXP, 0);
  }
  if (n == N) {
    return allocVector(n <= INTSXP ? INTSXP : REALSXP, N);
  }

  R_xlen_t j = 0;

  if (n <= INT_MAX) {
    SEXP ans = PROTECT(allocVector(INTSXP, n));
    int * restrict ansp = INTEGER(ans);
    switch(TYPEOF(x)) {
    case LGLSXP: {
      const int * xp = LOGICAL(x);
      SEXP ans = PROTECT(allocVector(INTSXP, n));
      int * restrict ansp = INTEGER(ans);
      if (isnt) {
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[j] = i + 1;
          j += xp[i] != NA_LOGICAL;
        }
      } else {
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[j] = i + 1;
          j += xp[i] == NA_LOGICAL;
        }
      }

    }
      break;
    case INTSXP: {
      const int * xp = INTEGER(x);
      if (isnt) {
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[j] = i + 1;
          j += xp[i] != NA_INTEGER;
        }
      } else {
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[j] = i + 1;
          j += xp[i] == NA_INTEGER;
        }
      }
    }
      break;
    case REALSXP: {
      const double * xp = REAL(x);
      if (isnt) {
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[j] = i + 1;
          j += !ISNAN(xp[i]);
        }
      } else {
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[j] = i + 1;
          j += ISNAN(xp[i]);
        }
      }
    }
      break;
    case STRSXP: {
      if (isnt) {
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[j] = i + 1;
        j += STRING_ELT(x, i) != NA_STRING;
      }
    } else {
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[j] = i + 1;
        j += STRING_ELT(x, i) == NA_STRING;
      }
    }
    }
      break;
    case CPLXSXP: {
      if (isnt) {
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[j] = i + 1;
        Rcomplex v = COMPLEX_ELT(x, i);
        j += !(ISNAN(v.r) || ISNAN(v.i));
      }
    } else {
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[j] = i + 1;
        Rcomplex v = COMPLEX_ELT(x, i);
        j += ISNAN(v.r) || ISNAN(v.i);
      }
    }
    }
    }
    UNPROTECT(1);
    return ans;
  }

  SEXP ans = PROTECT(allocVector(REALSXP, n));
  double * restrict ansp = REAL(ans);
  switch(TYPEOF(x)) {
  case LGLSXP: {

    const int * xp = LOGICAL(x);
    if (isnt) {
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[j] = i + 1;
        j += xp[i] != NA_LOGICAL;
      }
    } else {
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[j] = i + 1;
        j += xp[i] == NA_LOGICAL;
      }
    }

  }
    break;
  case INTSXP: {
    const int * xp = INTEGER(x);

    if (isnt) {
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[j] = i + 1;
        j += xp[i] != NA_INTEGER;
      }
    } else {
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[j] = i + 1;
        j += xp[i] == NA_INTEGER;
      }
    }
    UNPROTECT(1);
    return ans;
  }
    break;
  case REALSXP: {
    const double * xp = REAL(x);
    if (isnt) {
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[j] = i + 1;
        j += !ISNAN(xp[i]);
      }
    } else {
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[j] = i + 1;
        j += ISNAN(xp[i]);
      }
    }
    UNPROTECT(1);
    return ans;
  }
    break;
  case STRSXP: {
    if (isnt) {
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[j] = i + 1;
      j += STRING_ELT(x, i) != NA_STRING;
    }
  } else {
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[j] = i + 1;
      j += STRING_ELT(x, i) == NA_STRING;
    }
  }
  }
    break;
  case CPLXSXP: {
    if (isnt) {
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[j] = i + 1;
      Rcomplex v = COMPLEX_ELT(x, i);
      j += !(ISNAN(v.r) || ISNAN(v.i));
    }
  } else {
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[j] = i + 1;
      Rcomplex v = COMPLEX_ELT(x, i);
      j += ISNAN(v.r) || ISNAN(v.i);
    }
  }
  }

  }
  UNPROTECT(1);
  return ans;
}
