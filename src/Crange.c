#include "hutilscpp.h"

SEXP Crangel4(SEXP x) {
  R_xlen_t N = xlength(x);
  // # nocov start
  if (N == 0) {
    SEXP ans = PROTECT(allocVector(INTSXP, 4));
    INTEGER(ans)[0] = 1;
    INTEGER(ans)[1] = 0;
    INTEGER(ans)[2] = 0;
    INTEGER(ans)[3] = 0;
    UNPROTECT(1);
    return ans;
  }
  // # nocov end
  const int * xp = LOGICAL(x);
  bool has_true = false;
  bool has_false = false;
  R_xlen_t wmin = 1, wmax = 1;
  for (R_xlen_t i = 0; i < N; ++i) {
    if (has_true & has_false) {
      break;
    }
    switch(xp[i]) {
    case 0: {
      wmin = has_false ? wmin : i + 1;
      has_false = true;
    }
      break;
    case 1: {
      wmax = has_true ? wmax : i + 1;
      has_true = true;
    }
      break;
    }
  }

  if (wmin <= INT_MAX && wmax <= INT_MAX) {
    SEXP ans = PROTECT(allocVector(INTSXP, 4));
    if (!has_true && !has_false) {
      // empty
      INTEGER(ans)[0] = NA_LOGICAL;
      INTEGER(ans)[1] = NA_LOGICAL;
      INTEGER(ans)[2] = NA_LOGICAL;
      INTEGER(ans)[3] = NA_LOGICAL;
    } else {
      INTEGER(ans)[0] = has_false ? 0 : 1;
      INTEGER(ans)[1] = has_true ? 1 : 0;
      INTEGER(ans)[2] = wmin;
      INTEGER(ans)[3] = wmax;
    }
    UNPROTECT(1);
    return ans;
  }
  // # nocov start
  SEXP ans = PROTECT(allocVector(REALSXP, 4));
  REAL(ans)[0] = has_false ? 0 : 1;
  REAL(ans)[1] = has_true ? 1 : 0;
  REAL(ans)[2] = wmin;
  REAL(ans)[3] = wmax;
  UNPROTECT(1);
  return ans;
  // # nocov end
}

SEXP Crangei4(SEXP x) {
  R_xlen_t N = xlength(x);
  // # nocov start
  if (N == 0) {
    SEXP ans = PROTECT(allocVector(INTSXP, 4));
    INTEGER(ans)[0] = INT_MAX;
    INTEGER(ans)[1] = INT_MIN;
    INTEGER(ans)[2] = 0;
    INTEGER(ans)[3] = 0;
    UNPROTECT(1);
    return ans;
  }
  // # nocov end
  const int * xp = INTEGER(x);
  int xmin = xp[0], xmax = xp[0];
  R_xlen_t wmin = 1, wmax = 1;
  while (xmin == NA_INTEGER && wmin < N) {
    xmin = xp[wmin];
    ++wmin;
    ++wmax;
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    if (xpi < xmin) {
      if (xpi == NA_INTEGER) {
        continue;
      }
      wmin = i + 1;
      xmin = xpi;
    } else if (xpi > xmax) {
      wmax = i + 1;
      xmax = xpi;
    }
  }
  if (wmin <= INT_MAX && wmax <= INT_MAX) {
    SEXP ans = PROTECT(allocVector(INTSXP, 4));
    INTEGER(ans)[0] = xmin;
    INTEGER(ans)[1] = xmax;
    INTEGER(ans)[2] = wmin;
    INTEGER(ans)[3] = wmax;
    UNPROTECT(1);
    return ans;
  }
  // # nocov start
  SEXP ans = PROTECT(allocVector(REALSXP, 4));
  REAL(ans)[0] = xmin;
  REAL(ans)[1] = xmax;
  REAL(ans)[2] = wmin;
  REAL(ans)[3] = wmax;
  UNPROTECT(1);
  return ans;
  // # nocov end
}

SEXP Cranged4(SEXP x) {
  R_xlen_t N = xlength(x);
  // # nocov start
  if (N == 0) {
    SEXP ans = PROTECT(allocVector(REALSXP, 4));
    REAL(ans)[0] = R_PosInf;
    REAL(ans)[1] = R_NegInf;
    REAL(ans)[2] = 0;
    REAL(ans)[3] = 0;
    UNPROTECT(1);
    return ans;
  }
  // # nocov end
  const double * xp = REAL(x);
  double xp0 = xp[0];
  R_xlen_t j = 1;
  while (ISNAN(xp0) && j < N) {
    xp0 = xp[j];
    ++j;
  }

  double xmin = xp0, xmax = xp0;

  R_xlen_t wmin = j, wmax = j;
  for (R_xlen_t i = 0; i < N; ++i) {
    double xpi = xp[i];
    if (ISNAN(xpi)) {
      continue;
    }
    if (xpi < xmin) {
      wmin = i + 1;
      xmin = xpi;
    } else if (xpi > xmax) {
      wmax = i + 1;
      xmax = xpi;
    }
  }
  SEXP ans = PROTECT(allocVector(REALSXP, 4));
  REAL(ans)[0] = xmin;
  REAL(ans)[1] = xmax;
  REAL(ans)[2] = wmin;
  REAL(ans)[3] = wmax;
  UNPROTECT(1);
  return ans;
}

SEXP Crange(SEXP x) {
  switch(TYPEOF(x)) {
  case LGLSXP:
    return Crangel4(x);
  case INTSXP:
    return Crangei4(x);
  case REALSXP:
    return Cranged4(x);
  }
  return R_NilValue;
}

SEXP Crangel2_nanyNA(SEXP x, int nThread) {
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) != LGLSXP || xlength(x) == 0) {
    return R_NilValue;  // # nocov
  }
  const int * xp = LOGICAL(x);
  bool any_false = false;
  bool any_true = false;
  if (xp[0]) {
    any_true = true;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(|| : any_false)
#endif
    for (R_xlen_t i = 1; i < N; ++i) {
      any_false |= xp[i] == 0;
    }
  } else {
    any_false = true;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(|| : any_true)
#endif
    for (R_xlen_t i = 1; i < N; ++i) {
      any_true |= xp[i];
    }
  }
  SEXP ans = PROTECT(allocVector(LGLSXP, 2));
  LOGICAL(ans)[0] = any_false ? 0 : 1;
  LOGICAL(ans)[1] = any_true ? 1 : 0;
  UNPROTECT(1);
  return ans;
}

SEXP Cminmax(SEXP x, SEXP emptyResult, SEXP nthreads) {
  R_xlen_t N = xlength(x);
  if (N == 0) {
    return emptyResult;
  }
  int nThread = as_nThread(nthreads);

  switch(TYPEOF(x)) {
  case LGLSXP:
    return Crangel2_nanyNA(x, nThread);
  case INTSXP: {
    const int * xp = INTEGER(x);
    int xmin = xp[0];
    int xmax = xp[0];
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(min : xmin) reduction(max : xmax)
#endif
    for (R_xlen_t i = 1; i < N; ++i) {
      int xi = xp[i];
      bool nochange = xi >= xmin && xi <= xmax;
      if (nochange) continue;
      xmin = (xi < xmin) ? xi : xmin;
      xmax = (xi > xmax) ? xi : xmax;
    }
    SEXP ans = PROTECT(allocVector(INTSXP, 2));
    INTEGER(ans)[0] = xmin;
    INTEGER(ans)[1] = xmax;
    UNPROTECT(1);
    return ans;
  }
    break;
  case REALSXP: {
    const double * xp = REAL(x);
    double xmin = xp[0];
    double xmax = xp[0];
    if (ISNAN(xmin)) {
      xmin = R_PosInf;
    }
    if (ISNAN(xmax)) {
      xmax = R_NegInf;
    }
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(min : xmin) reduction(max : xmax)
#endif
    for (R_xlen_t i = 1; i < N; ++i) {
      double xi = xp[i];
      bool nochange = xi >= xmin && xi <= xmax;
      if (nochange || ISNAN(xi)) continue;
      xmin = (xi < xmin) ? xi : xmin;
      xmax = (xi > xmax) ? xi : xmax;
    }
    SEXP ans = PROTECT(allocVector(REALSXP, 2));
    REAL(ans)[0] = xmin;
    REAL(ans)[1] = xmax;
    UNPROTECT(1);
    return ans;
  }
    break;
  case STRSXP: {
    const char * xmin = CHAR(STRING_ELT(x, 0));
    const char * xmax = CHAR(STRING_ELT(x, 0));
    for (R_xlen_t i = 1; i < N; ++i) {
      const char * xi = CHAR(STRING_ELT(x, i));
      xmin = strcmp(xi, xmin) < 0 ? xi : xmin;
      xmax = strcmp(xi, xmax) > 0 ? xi : xmax;
    }
    SEXP ans = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(ans, 0, Rf_mkChar(xmin));
    SET_STRING_ELT(ans, 1, Rf_mkChar(xmax));
    UNPROTECT(1);
    return ans;
  }
    break;
  case RAWSXP: {
    const unsigned char * xp = RAW(x);
    unsigned char xmax = 0;
    unsigned char xmin = 255;
    // using bool caused problems in clang19, perhaps because it treats it as i8
    unsigned int o[256] = {0};
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(|| : o[:256])
#endif
    for (R_xlen_t i = 0; i < N; ++i) {
      int xi = xp[i];
      o[xi] = true;
    }
    for (int i = 0; i < 256; ++i) {
      if (o[i]) {
        xmin = (unsigned char)i;
        break;
      }
    }
    for (int i = 255; i >= xmin; --i) {
      if (o[i]) {
        xmax = (unsigned char)i;
        break;
      }
    }

    SEXP ans = PROTECT(allocVector(RAWSXP, 2));
    RAW(ans)[0] = xmin;
    RAW(ans)[1] = xmax;
    UNPROTECT(1);
    return ans;
  }

  }
  return R_NilValue; // # nocov
}
