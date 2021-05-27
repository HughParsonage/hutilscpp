#include "hutilscpp.h"

bool do_is_safe2int(double x) {
  return !ISNAN(x) && x <= 2147483647 && x >= -2147483647 && ((int)x == x);
}

//' @noRd
//' @param x Candidate vector.
//' @return
//'   0 if unsafe to coerce to integer
//'   1 if   safe to coerce to integer and _zero_ NAs in output
//'   2 if   safe to coerce to integer but _some_ NAs in output
int dbl_is_int(double x) {
  if (ISNAN(x)) {
    return 2; // # nocov
  }
  if (x > 2147483647 || x < -2147483647) {
    return 0;
  }
  int xi = (int)x;
  if (xi != x) {
    return 0;
  }
  return 1;
}


int dbl2int(double x) {
  if (dbl_is_int(x) != 1) {
    return NA_INTEGER;
  }
  return (int)x;
}

int sex2int1(SEXP x) {
  if (xlength(x) != 1) {
    return NA_INTEGER;
  }
  switch(TYPEOF(x)) {
  case LGLSXP:
  case INTSXP:
    return INTEGER_ELT(x, 0);
  case REALSXP:
    return dbl2int(REAL_ELT(x, 0));
  }
  return NA_INTEGER; // # nocov
}

SEXP Cwhich_isnt_integerish(SEXP xx) {
  // handled at R level
  // # nocov start
  if (TYPEOF(xx) == INTSXP || xlength(xx) == 0) {
    return ScalarInteger(0);
  }
  if (TYPEOF(xx) != REALSXP) {
    return ScalarInteger(1);
  }
  // # nocov end
  R_xlen_t N = xlength(xx);
  const double * xp = REAL(xx);
  for (R_xlen_t i = 0; i < N; ++i) {
    if (!do_is_safe2int(xp[i])) {
      return ScalarLength(i + 1);
    }
  }
  return ScalarInteger(0);
}

int is_safe2int(SEXP x) {
  if (TYPEOF(x) == INTSXP) {
    return 2;
  }
  if (TYPEOF(x) != REALSXP) {
    return 0;
  }
  R_xlen_t n = xlength(x);
  int out = 1; // Purpose of loop is to contradict this
  double * xp = REAL(x);
  for (R_xlen_t i = 0; i < n; ++i) {
    double xi = xp[i];
    // (int)NaN is UBD

    if (!ISNAN(xi) && xi <= 2147483647 && xi >= -2147483647) {
      int xint = (int)xi;
      if (xint != xi) {
        out = 0; // integer not possible
        break;
      }
      continue;
    }
    if (R_IsNA(xi) || R_IsNaN(xi)) {
      out = 2; // out = 1 no longer possible
      continue;
    }
    if (!R_finite(xi)) {
      out = 0;
    }
    if (xi > 2147483647) {
      out = 0;
      break;
    } else if (xi + 2147483647 <= 0) {
      out = 0;
      break;
    }
  }
  return out;
}

SEXP Cis_safe2int(SEXP x) {
  return ScalarInteger(is_safe2int(x));
}

SEXP Cforce_as_integer(SEXP xx, SEXP Na_code) {
  if (TYPEOF(xx) == INTSXP) {
    return xx; // # nocov
  }
  if (TYPEOF(xx) == LGLSXP) {
    R_xlen_t N = xlength(xx);
    const int * xp = LOGICAL(xx);
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * ansp = INTEGER(ans);
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = xp[i];
    }
    UNPROTECT(1);
    return ans;
  }
  if (TYPEOF(xx) != REALSXP || TYPEOF(Na_code) != INTSXP) {
    return R_NilValue; // # nocov
  }
  int na_code = asInteger2(Na_code);
  if (na_code < 0 || na_code > 2) {
    na_code = is_safe2int(xx); // # nocov
  }
  if (na_code != 1 && na_code != 2) {
    error("x could not be safely coerced to integer.");
  }
  R_xlen_t N = xlength(xx);
  const double * x = REAL(xx);
  SEXP out = PROTECT(allocVector(INTSXP, N));
  int * restrict outp = INTEGER(out);
  // (int) non-finites are UBD
  switch(na_code) {
  case 1:
    for (R_xlen_t i = 0; i < N; ++i) {
      outp[i] = (int)x[i];
    }
  case 2:
    for (R_xlen_t i = 0; i < N; ++i) {
      outp[i] = (R_finite(x[i])) ? (int)x[i] : NA_INTEGER;
    }
  }
  UNPROTECT(1);
  return out;
}

