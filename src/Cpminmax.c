#include "hutilscpp.h"



R_xlen_t do_firstNonNegativeRadix_int(const int * x,
                                      R_xlen_t xsize,
                                      R_xlen_t mini,
                                      R_xlen_t maxi,
                                      bool desc,
                                      int depth) {
  if (maxi < 0 || maxi > xsize) {
    if (xsize < 1) {
      return xsize; // # nocov
    }
    maxi = xsize;
  }
  if (mini < 0) {
    mini = 0;
  }
  int lastx = x[maxi - 1];

  if (desc) {
    if (x[mini] < 0 || lastx > 0) {
      return mini;
    }
  } else {
    if (x[mini] > 0 || lastx < 0) {
      return mini;
    }
  }

  if (mini > maxi - 1024 || depth > 31) {
    for (R_xlen_t i = mini; i < maxi; ++i) {
      if (desc) {
        if (x[i] <= 0) {
          return i;
        }
      } else {
        if (x[i] >= 0) {
          return i;
        }
      }
    }
    return maxi; // # nocov
  }
  R_xlen_t medi = mini + (maxi - mini) / 2;
  bool lhs = (x[medi] < 0) ? desc : !desc;
  R_xlen_t left = lhs ? mini : medi - 1;
  R_xlen_t right = lhs ? medi + 2 : maxi;
  return do_firstNonNegativeRadix_int(x, xsize, left, right, desc, depth + 1);
}

R_xlen_t do_firstNonNegativeRadix_dbl(const double * x,
                                      R_xlen_t xsize,
                                      R_xlen_t mini,
                                      R_xlen_t maxi,
                                      bool desc,
                                      int depth) {
  if (maxi < 0 || maxi > xsize) {
    if (xsize < 1) {
      return 0; // # nocov
    }
    maxi = xsize;
  }
  if (mini < 0) {
    mini = 0;
  }
  double lastx = x[maxi - 1];


  if (desc) {
    if (x[mini] < 0 || lastx > 0) {
      return mini;
    }
  } else {
    if (x[mini] > 0 || lastx < 0) {
      return mini;
    }
  }

  if (mini > maxi - 1024 || depth > 31) {
    // showValuex("depth = ", depth);
    for (R_xlen_t i = mini; i < maxi; ++i) {
      if (desc) {
        if (x[i] <= 0) {
          return i;
        }
      } else {
        if (x[i] >= 0) {
          return i;
        }
      }
    }
    return maxi; // # nocov
  }
  R_xlen_t medi = mini + (maxi - mini) / 2;
  bool lhs = (x[medi] < 0) ? desc : !desc;
  R_xlen_t left = lhs ? mini : medi - 1;
  R_xlen_t right = lhs ? medi + 1 : maxi;
  return do_firstNonNegativeRadix_dbl(x, xsize, left, right, desc, depth + 1);
}

SEXP CfirstNonNegativeRadix(SEXP x, SEXP Mini, SEXP Maxi, SEXP Desc) {
  // (const double * x,
  //  R_xlen_t xsize,
  //  R_xlen_t mini,
  //  R_xlen_t maxi,
  //  bool desc,
  //  int depth)

  R_xlen_t N = xlength(x);
  R_xlen_t mini = TYPEOF(Mini) == INTSXP ? asInteger(Mini) : asReal(Mini);
  R_xlen_t maxi = TYPEOF(Maxi) == INTSXP ? asInteger(Maxi) : asReal(Maxi);
  bool desc = asLogical(Desc);
  R_xlen_t o = 0;
  if (TYPEOF(x) == INTSXP) {
    const int * xp = INTEGER(x);
    o = do_firstNonNegativeRadix_int(xp, N, mini, maxi, desc, 0);
  } else if (TYPEOF(x) == REALSXP) {
    const double * xp = REAL(x);
    o = do_firstNonNegativeRadix_dbl(xp, N, mini, maxi, desc, 0);
  } else {
    return R_NilValue; // # nocov
  }
  return ScalarLength(o);
}

SEXP Cpmax0_radix_sorted_dbl(SEXP xx,
                             SEXP InPlace,
                             SEXP nthreads) {
  if (TYPEOF(xx) != REALSXP ||
      TYPEOF(InPlace) != LGLSXP ||
      xlength(InPlace) != 1) {
    return R_NilValue; // # nocov
  }
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(xx);
  double * x = REAL(xx);
  if (N == 0) {
    return xx;
  }
  if (N == 1) {
    return (x[0] >= 0) ? xx : ScalarReal(0);
  }
  bool x0_positive = x[0] > 0;
  bool xn_positive = x[N - 1] > 0;
  if (x0_positive && xn_positive) {
    return xx;
  }
  bool in_place = asLogical(InPlace);
  if (in_place) {
    if (!x0_positive && !xn_positive) {
      for (R_xlen_t i = 0; i < N; ++i) {
        x[i] = 0;
      }
    } else {
      for (R_xlen_t i = 0; i < N; ++i) {
        double xi = x[i];
        if (xi < 0) {
          x[i] = 0;
        }
      }
    }
    return xx;
  }
  if (!x0_positive && !xn_positive) {
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * ansp = REAL(ans);
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = 0;
    }
    UNPROTECT(1);
    return ans;
  }
  bool desc = x[0] > 0;
  R_xlen_t root = do_firstNonNegativeRadix_dbl(x, N, 0, N, desc, 0);

  SEXP out = PROTECT(allocVector(REALSXP, N));
  double * restrict outp = REAL(out);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    bool zero = desc ? i >= root : i < root;
    outp[i] = zero ? 0 : x[i];
  }
  UNPROTECT(1);
  return out;
}

SEXP Cpmin0_radix_sorted_dbl(SEXP xx,
                             SEXP InPlace,
                             SEXP nthreads) {
  if (TYPEOF(xx) != REALSXP) {
    return R_NilValue; // # nocov
  }
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(xx);
  double * x = REAL(xx);
  bool x0_positive = x[0] > 0;
  bool xn_positive = x[N - 1] > 0;
  if (!x0_positive && !xn_positive) {
    return xx;
  }
  bool in_place = is_true(InPlace);
  if (in_place) {
    if (x0_positive && xn_positive) {
      for (R_xlen_t i = 0; i < N; ++i) {
        x[i] = 0;
      }
    } else {
      for (R_xlen_t i = 0; i < N; ++i) {
        double xi = x[i];
        if (xi > 0) {
          x[i] = 0;
        }
      }
    }
    return xx;
  }

  if (x0_positive && xn_positive) {
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * ansp = REAL(ans);
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = 0;
    }
    UNPROTECT(1);
    return ans;
  }
  const bool desc = x[0] > 0;
  R_xlen_t root = do_firstNonNegativeRadix_dbl(x, N, 0, N, desc, 0);
  SEXP out = PROTECT(allocVector(REALSXP, N));
  double * restrict outp = REAL(out);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    bool zero = desc ? i < root : i >= root;
    outp[i] = zero ? 0 : x[i];
  }
  UNPROTECT(1);
  return out;
}


SEXP Cpmax0_radix_sorted_int(SEXP xx,
                             SEXP InPlace,
                             SEXP nthreads) {
  if (TYPEOF(xx) != INTSXP) {
    return R_NilValue; // # nocov
  }
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(xx);
  bool in_place = is_true(InPlace);
  int * x = INTEGER(xx);
  bool x0_positive = x[0] > 0;
  bool xn_positive = x[N - 1] > 0;
  if (x0_positive && xn_positive) {
    return xx;
  }
  if (in_place) {
    if (!x0_positive && !xn_positive) {
      for (R_xlen_t i = 0; i < N; ++i) {
        x[i] = 0;
      }
    } else {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = x[i];
        if (xi < 0) {
          x[i] = 0;
        }
      }
    }
    return xx;
  }
  if (!x0_positive && !xn_positive) {
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * ansp = INTEGER(ans);
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = 0;
    }
    UNPROTECT(1);
    return ans;
  }
  bool desc = x[0] > 0;
  R_xlen_t root = do_firstNonNegativeRadix_int(x, N, 0, N, desc, 0);
  SEXP out = PROTECT(allocVector(INTSXP, N));
  int * restrict outp = INTEGER(out);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    bool zero = desc ? i >= root : i < root;
    outp[i] = zero ? 0 : x[i];
  }
  UNPROTECT(1);
  return out;
}

SEXP Cpmin0_radix_sorted_int(SEXP xx,
                             SEXP InPlace,
                             SEXP nthreads) {
  if (TYPEOF(xx) != INTSXP ||
      TYPEOF(InPlace) != LGLSXP ||
      xlength(InPlace) != 1) {
    return R_NilValue; // # nocov
  }
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(xx);
  bool in_place = asLogical(InPlace);
  int * x = INTEGER(xx);
  bool x0_positive = x[0] > 0;
  bool xn_positive = x[N - 1] > 0;
  if (!x0_positive && !xn_positive) {
    return xx;
  }
  if (in_place) {
    if (x0_positive && xn_positive) {
      for (R_xlen_t i = 0; i < N; ++i) {
        x[i] = 0;
      }
    } else {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = x[i];
        if (xi > 0) {
          x[i] = 0;
        }
      }
    }
    return xx;
  }

  if (x0_positive && xn_positive) {
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * ansp = INTEGER(ans);
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = 0;
    }
    UNPROTECT(1);
    return ans;
  }
  const bool desc = x[0] > 0;
  R_xlen_t root = do_firstNonNegativeRadix_int(x, N, 0, N, desc, 0);
  SEXP out = PROTECT(allocVector(INTSXP, N));
  int * restrict outp = INTEGER(out);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    bool zero = desc ? i < root : i >= root;
    outp[i] = zero ? 0 : x[i];
  }
  UNPROTECT(1);
  return out;
}

SEXP Cpmax0_bitwise(SEXP xx, SEXP InPlace, SEXP nthreads) {
  if (TYPEOF(xx) != INTSXP) {
    return R_NilValue; // # nocov
  }
  bool in_place = asLogical(InPlace);
  R_xlen_t N = xlength(xx);
  if (in_place) {
    int * x = INTEGER(xx);
    R_xlen_t j = 0;
    while (j < N && x[j] >= 0) {
      ++j;
    }
    if (j == N) {
      return xx;
    }
    for (R_xlen_t i = j; i < N; ++i) {
      int xi = x[i];
      int r = xi - (xi & (xi >> (sizeof(int) * CHAR_BIT - 1)));
      x[i] = r;
    }
    return xx;
  }
  int nThread = as_nThread(nthreads);
  R_xlen_t j = 0;
  const int * x = INTEGER(xx);
  while (j < N && x[j] >= 0) {
    ++j;
  }
  if (j == N) {
    return xx;
  }

  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict out = INTEGER(ans);

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    if (i < j) {
      out[i] = x[i];
      continue;
    }
    // https://graphics.stanford.edu/~seander/bithacks.html#IntegerMinOrMax
    int xi = x[i];
    int r = xi - (xi & (xi >> (sizeof(int) * CHAR_BIT - 1)));
    out[i] = r;
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cpmin0_bitwise(SEXP xx,
                    SEXP InPlace,
                    SEXP nthreads) {
  if (TYPEOF(xx) != INTSXP) {
    return R_NilValue; // # nocov
  }
  bool in_place = asLogical(InPlace);
  R_xlen_t N = xlength(xx);
  if (in_place) {
    int * x = INTEGER(xx);
    R_xlen_t j = 0;
    while (j < N && x[j] <= 0) {
      ++j;
    }
    if (j == N) {
      return xx;
    }
    for (R_xlen_t i = j; i < N; ++i) {
      int xi = x[i];
      int r = (xi & (xi >> (sizeof(int) * CHAR_BIT - 1)));
      x[i] = r;
    }
    return xx;
  }
  int nThread = as_nThread(nthreads);

  const int * x = INTEGER(xx);
  R_xlen_t j = 0;
  while (j < N && x[j] <= 0) {
    ++j;
  }
  if (j == N) {
    return xx;
  }

  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict out = INTEGER(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    // https://graphics.stanford.edu/~seander/bithacks.html#IntegerMinOrMax
    int xi = x[i];
    if (i < j) {
      out[i] = xi;
      continue;
    }
    int r = (xi & (xi >> (sizeof(int) * CHAR_BIT - 1)));
    out[i] = r;
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cpmax(SEXP x, SEXP y, SEXP keepNas, SEXP nthreads) {
  R_xlen_t N = xlength(x);
  int nThread = as_nThread(nthreads);
  bool keep_nas = asLogical(keepNas);
  // int switcher =
  //   (TYPEOF(x) == REALSXP) +
  //   2 * (xlength(y) == N) +
  //   4 * (keep_nas);
  if (TYPEOF(x) == INTSXP &&
      TYPEOF(y) == INTSXP &&
      xlength(y) == 1) {
    const int * xp = INTEGER(x);
    const int a = asInteger(y);
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * restrict ansp = INTEGER(ans);
    if (keep_nas) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = xp[i] == NA_INTEGER ? NA_INTEGER : maxii(xp[i], a);
      }
    } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = maxii(xp[i], a);
      }
    }
    UNPROTECT(1);
    return ans;
  }
  if (TYPEOF(x) == INTSXP &&
      TYPEOF(y) == INTSXP &&
      xlength(y) == N) {
    const int * xp = INTEGER(x);
    const int * yp = INTEGER(y);
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * restrict ansp = INTEGER(ans);
    if (keep_nas) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = (xp[i] == NA_INTEGER || yp[i] == NA_INTEGER) ? NA_INTEGER : maxii(xp[i], yp[i]);
      }
    } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = maxii(xp[i], yp[i]);
      }
    }
    UNPROTECT(1);
    return ans;
  }
  if (TYPEOF(x) == INTSXP &&
      TYPEOF(y) == REALSXP &&
      xlength(y) == 1) {
    const int * xp = INTEGER(x);
    const double ad = asReal(y);

    int dbl_is_inti = dbl_is_int(ad);
    switch(dbl_is_inti) {
    case 0: {
      SEXP ans = PROTECT(allocVector(REALSXP, N));
      double * restrict ansp = REAL(ans);
      if (keep_nas) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = xp[i] == NA_INTEGER ? NA_REAL : maxid(xp[i], ad);
        }
      } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = maxid(xp[i], ad);
        }
      }
      UNPROTECT(1);
      return ans;
    }
      // normally handled at R level
      // # nocov start
      break;
    case 2:
      // NA
      if (keep_nas) {
        SEXP ans = PROTECT(allocVector(INTSXP, N));
        int * restrict ansp = INTEGER(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = NA_INTEGER;
        }
        UNPROTECT(1);
        return ans;
      }
      break;
      // # nocov end
    case 1: {
        const int a = (int)ad;
        SEXP ans = PROTECT(allocVector(INTSXP, N));
        int * restrict ansp = INTEGER(ans);
        if (keep_nas) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
          for (R_xlen_t i = 0; i < N; ++i) {
            ansp[i] = xp[i] == NA_INTEGER ? NA_INTEGER: maxii(xp[i], a);
          }
        } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
          for (R_xlen_t i = 0; i < N; ++i) {
            ansp[i] = maxii(xp[i], a);
          }
        }
        UNPROTECT(1);
        return ans;
      }
    }
  }
  if (TYPEOF(x) == INTSXP &&
      TYPEOF(y) == REALSXP &&
      xlength(y) == N) {
    const int * xp = INTEGER(x);
    const double * yp = REAL(y);
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * restrict ansp = REAL(ans);
    if (keep_nas) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = maxid(xp[i], yp[i]);
      }
    } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = maxid(xp[i], yp[i]);
      }
    }
    UNPROTECT(1);
    return ans;
  }
  if (TYPEOF(x) == REALSXP &&
      TYPEOF(y) == INTSXP &&
      xlength(y) == N) {
    const double * xp = REAL(x);
    const int * yp = INTEGER(y);
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * restrict ansp = REAL(ans);
    if (keep_nas) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = (ISNAN(xp[i]) || yp[i] == NA_INTEGER) ? NA_REAL : maxid(yp[i], xp[i]);
      }
    } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = maxid(yp[i], xp[i]);
      }
    }
    UNPROTECT(1);
    return ans;
  }
  if (TYPEOF(x) == REALSXP &&
      TYPEOF(y) == REALSXP &&
      xlength(y) == 1) {
    const double * xp = REAL(x);
    const double a = asReal(y);
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * restrict ansp = REAL(ans);
    if (keep_nas) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = ISNAN(xp[i]) ? NA_REAL : maxdd(xp[i], a);
      }
    } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = maxdd(xp[i], a);
      }
    }
    UNPROTECT(1);
    return ans;
  }
  if (TYPEOF(x) == REALSXP &&
      TYPEOF(y) == REALSXP &&
      xlength(y) == N) {
    const double * xp = REAL(x);
    const double * yp = REAL(y);
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * restrict ansp = REAL(ans);
    if (keep_nas) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = (ISNAN(xp[i]) || ISNAN(yp[i])) ? NA_REAL : maxdd(yp[i], xp[i]);
      }
    } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = maxdd(yp[i], xp[i]);
      }
    }
    UNPROTECT(1);
    return ans;
  }
  return R_NilValue; // # nocov
}



SEXP Cpmin(SEXP x, SEXP y, SEXP keepNas, SEXP nthreads) {
  R_xlen_t N = xlength(x);
  int nThread = as_nThread(nthreads);
  bool keep_nas = asLogical(keepNas);
  if (TYPEOF(x) == INTSXP &&
      TYPEOF(y) == INTSXP &&
      xlength(y) == 1) {
    const int * xp = INTEGER(x);
    const int a = asInteger(y);
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * restrict ansp = INTEGER(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = minii(xp[i], a);
    }

    UNPROTECT(1);
    return ans;
  }
  if (TYPEOF(x) == INTSXP &&
      TYPEOF(y) == INTSXP &&
      xlength(y) == N) {
    const int * xp = INTEGER(x);
    const int * yp = INTEGER(y);
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * restrict ansp = INTEGER(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = minii(xp[i], yp[i]);
    }
    UNPROTECT(1);
    return ans;
  }
  if (TYPEOF(x) == INTSXP &&
      TYPEOF(y) == REALSXP &&
      xlength(y) == 1) {
    const int * xp = INTEGER(x);
    const double ad = asReal(y);
    int adi = dbl_is_int(ad);
    switch(adi) {
    case 0: {
      SEXP ans = PROTECT(allocVector(REALSXP, N));
      double * restrict ansp = REAL(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = minid(xp[i], ad);
      }
      UNPROTECT(1);
      return ans;
    }
      break;
      // # nocov start
    case 2:
      // NA
      if (keep_nas) {
        return IntegerNNA(N);
      } else {
        return x;
      }
      break;
    case 1: {
        const int a = (int)ad;
        SEXP ans = PROTECT(allocVector(INTSXP, N));
        int * restrict ansp = INTEGER(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] = minii(xp[i], a);
        }
        UNPROTECT(1);
        return ans;

      }
      // # nocov end
    }
  }
  if (TYPEOF(x) == INTSXP &&
      TYPEOF(y) == REALSXP &&
      xlength(y) == N) {
    const int * xp = INTEGER(x);
    const double * yp = REAL(y);
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * restrict ansp = REAL(ans);
    if (keep_nas) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = (xp[i] == NA_INTEGER || ISNAN(yp[i])) ? NA_REAL : minid(xp[i], yp[i]);
      }
    } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = minid(xp[i], yp[i]);
      }
    }
    UNPROTECT(1);
    return ans;
  }
  if (TYPEOF(x) == REALSXP &&
      TYPEOF(y) == INTSXP &&
      xlength(y) == N) {
    const double * xp = REAL(x);
    const int * yp = INTEGER(y);
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * restrict ansp = REAL(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = minid(yp[i], xp[i]);
    }
    UNPROTECT(1);
    return ans;
  }
  if (TYPEOF(x) == REALSXP &&
      TYPEOF(y) == REALSXP &&
      xlength(y) == 1) {
    const double * xp = REAL(x);
    const double a = asReal(y);
    // # nocov start
    if (ISNAN(a)) {
      if (keep_nas) {
        return DoubleNNA(N);
      } else {
        return x;
      }
    }
    // # nocov end
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * restrict ansp = REAL(ans);
    if (keep_nas) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = ISNAN(xp[i]) ? NA_REAL : mindd(xp[i], a);
      }
    } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = mindd(xp[i], a);
      }
    }
    UNPROTECT(1);
    return ans;
  }
  if (TYPEOF(x) == REALSXP &&
      TYPEOF(y) == REALSXP &&
      xlength(y) == N) {
    const double * xp = REAL(x);
    const double * yp = REAL(y);
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * restrict ansp = REAL(ans);
    if (keep_nas) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = (ISNAN(yp[i]) || ISNAN(xp[i])) ? NA_REAL : mindd(yp[i], xp[i]);
      }
    } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (R_xlen_t i = 0; i < N; ++i) {
        ansp[i] = mindd(yp[i], xp[i]);
      }
    }
    UNPROTECT(1);
    return ans;
  }
  return R_NilValue; // # nocov
}

SEXP CpmaxC_in_place(SEXP x, SEXP a, SEXP keepNas, SEXP nthreads) {
  if (xlength(a) != 1) {
    return R_NilValue; // # nocov
  }

  R_xlen_t N = xlength(x);
  const bool keep_nas = is_true(keepNas);
  if (TYPEOF(x) == INTSXP &&
      TYPEOF(a) == INTSXP) {
    int * xp = INTEGER(x);
    int aa = asInteger(a);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xp[i] <= aa) {
        xp[i] = aa;
      }
    }
  }
  if (TYPEOF(x) == INTSXP &&
      TYPEOF(a) == REALSXP) {
    int * xp = INTEGER(x);
    double ad = asReal(a);
    // # nocov start
    switch(dbl_is_int(ad)) {
    case 0:
      return Cpmax(x, a, keepNas, nthreads);
    case 2:
      return keep_nas ? IntegerNNA(N) : x;
    case 1:
      break;
    }
    // # nocov end
    int aa = (int)(ad);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xp[i] <= aa) {
        xp[i] = aa;
      }
    }
  }
  if (TYPEOF(x) == REALSXP &&
      TYPEOF(a) == REALSXP) {
    double * xp = REAL(x);
    double aa = asReal(a);
    if (ISNAN(aa)) {
      return keep_nas ? DoubleNNA(N) : x; // # nocov
    }
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xp[i] <= aa) {
        xp[i] = aa;
      }
    }
  }
  return x;
}

SEXP CpminC_in_place(SEXP x, SEXP a, SEXP keepNas, SEXP nthreads) {
  if (xlength(a) != 1) {
    return R_NilValue; // # nocov
  }

  R_xlen_t N = xlength(x);
  const bool keep_nas = asLogical(keepNas);
  if (TYPEOF(x) == INTSXP &&
      TYPEOF(a) == INTSXP) {
    int * xp = INTEGER(x);
    int aa = asInteger(a);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xp[i] >= aa) {
        xp[i] = aa;
      }
    }
  }
  if (TYPEOF(x) == REALSXP &&
      TYPEOF(a) == REALSXP) {
    double * xp = REAL(x);
    double aa = asReal(a);
    if (ISNAN(aa)) {
      return keep_nas ? DoubleNNA(N) : x; // # nocov
    }
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xp[i] >= aa) {
        xp[i] = aa;
      }
    }
  }
  return x;
}
