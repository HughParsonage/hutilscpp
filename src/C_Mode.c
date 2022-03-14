#include "hutilscpp.h"

static int which_slice(int xpi) {
  if (xpi < 0) {
    if (xpi < -1073741823) {
      if (xpi < -1610612735) {
        return 0;
      } else {
        return 1;
      }
    } else {
      if (xpi < -536870911) {
        return 2;
      } else {
        return 3;
      }
    }
  } else {
    if (xpi < 1073741823) {
      if (xpi < 536870911) {
        return 4;
      } else {
        return 5;
      }
    } else {
      if (xpi < 1610612735) {
        return 6;
      } else {
        return 7;
      }
    }
  }
}

static int midpoint(int xmin, int xmax) {
  unsigned int r = (int64_t)xmax - (int64_t)xmin;
  return xmin + (r >> 1u);
}

static void do_minmnax_mode(uint64_t ans[2],
                            uint64_t * tbl,
                            const int * xp, R_xlen_t N, int nThread,
                            int xmin, int xmax) {
  int64_t xrange_t = (int64_t)xmax - (int64_t)xmin;
  if (xrange_t > 1e9) {
    int xmidpoint = midpoint(xmin, xmax);

    // split
    uint64_t ans_lhs[2] = {0};
    do_minmnax_mode(ans_lhs, tbl, xp, N, nThread, xmin, xmidpoint);

    uint64_t ans_rhs[2] = {0};
    do_minmnax_mode(ans_rhs, tbl, xp, N, nThread, xmidpoint, xmax);

    if (ans_lhs[1] >= ans_rhs[1]) {
      // first mode is ok
      ans[0] = ans_lhs[0];
      ans[1] = ans_lhs[1];
    } else {
      ans[0] = ans_rhs[0];
      ans[1] = ans_rhs[1];
    }
    return;
  }
  unsigned int range = xmax + 1u;

  range += ((unsigned int)(-xmin));
  memset(tbl, 0, range * sizeof(uint64_t));


  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    unsigned int t_j = (int64_t)xpi - (int64_t)xmin;
    if (t_j < range) {
      tbl[t_j]++;
    }
  }
  uint64_t tmax = 0; // possibly zero instances
  int ans0 = 0;
  for (unsigned int t = 0; t < range; ++t) {
    if (tbl[t] > tmax) {
      tmax = tbl[t];
      ans0 = t;
    }
  }
  ans[0] = (unsigned int)(ans0 + xmin);
  ans[1] = tmax;
}

SEXP C_Gross29_Count(SEXP x, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  if (!isInteger(x)) {
    error("not integer."); // # nocov
  }
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  uint64_t MacroCounts[8] = {0};

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : MacroCounts)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    MacroCounts[which_slice(xpi)]++;
  }

  SEXP ans = PROTECT(allocVector(REALSXP, 8));
  double * ansp = REAL(ans);
  for (int i = 0; i < 8; ++i) {
    ansp[i] = MacroCounts[i];
  }
  UNPROTECT(1);
  return ans;
}

SEXP C_Mode_via_ranges(SEXP x, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  if (!isInteger(x)) {
    error("not integer.");
  }
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);

  int SliceMins[8] = {-2147483647, -1610612735, -1073741823, -536870911, 0, 536870911, 1073741823, 1610612735};
  int SliceMaxs[8] = {-1610612735, -1073741823, -536870911, 0, 536870911, 1073741823, 1610612735, 2147483647};
  int Modes[8] = {0};
  uint64_t Counts[8] = {0};
  uint64_t MacroCounts[8] = {0};

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : MacroCounts)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    MacroCounts[which_slice(xpi)]++;
  }

  uint64_t * tbl = malloc(sizeof(uint64_t) * 536870913);
  if (tbl == NULL) {
    return R_NilValue;
  }
  int ans = Modes[0];
  uint64_t ans_count = 1;
  for (int slice = 0; slice < 8; ++slice) {
    if (MacroCounts[slice] < ans_count) {
      continue; // no chance of mode being present in a slice of fewer elements
    }
    uint64_t ModeCounts[2] = {0};
    do_minmnax_mode(ModeCounts, tbl, xp, N, nThread, SliceMins[slice], SliceMaxs[slice]);
    Modes[slice] = ModeCounts[0];
    Counts[slice] = ModeCounts[1];
    if (ans_count < Counts[slice]) {
      ans_count = Counts[slice];
      ans = Modes[slice];
    }
  }
  free(tbl);
  return ScalarInteger(ans);
}


SEXP C_Mode(SEXP x, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  if (!isInteger(x)) {
    error("not integer.");
  }
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  if (N <= 256) {
    unsigned int tbl[256] = {0};
    bool dup[256] = {0};
    tbl[0] = 1;
    for (R_xlen_t i = 0; i < N; ++i) {
      if (dup[i]) {
        continue;
      }
      int xpi = xp[i];
      for (R_xlen_t j = i + 1; j < N; ++j) {
        int xpj = xp[j];
        if (xpj == xpi) {
          dup[j] = true;
          tbl[i]++;
        }
      }
    }
    int ans = 0;
    int tbl_max = 1;
    for (int k = 0; k < 256; ++k) {
      if (tbl[k] > tbl_max) {
        ans = k;
        tbl_max = tbl[k];
      }
    }
    return ScalarInteger(xp[ans]);
  }

  int xmin = xp[0];
  int xmax = xp[0];

  FORLOOP_xminmax({
    int xpi = xp[i];
    if (xpi < xmin) {
      xmin = xpi;
    } else if (xpi > xmax) {
      xmax = xpi;
    }
  };)

  R_xlen_t xrange_t = (R_xlen_t)xmax - (R_xlen_t)xmin;
  if (xrange_t > 2e9) {
    uint64_t ans[2] = {0};
    uint64_t * tbl = malloc(sizeof(uint64_t) * 1e9);
    do_minmnax_mode(ans, tbl, xp, N, nThread, xmin, xmax);
    free(tbl);
    return ScalarInteger((unsigned int)ans[0]);
  }
  unsigned int range = xmax + 1u;
  range += ((unsigned int)(-xmin));
  if ((range >> 2) > N) {
    return R_NilValue;
  }

  unsigned int * tbl = malloc(sizeof(int) * range);
  if (tbl == NULL) {
    free(tbl);
    error("tbl could not be malloc'd."); // # nocov
  }
  memset(tbl, 0, sizeof(int) * range);

  for (R_xlen_t i = 0; i < N; ++i) {
    int64_t xpi = xp[i];
    int64_t di = xpi - xmin;
    tbl[di]++;
  }

  int tmax = 1;
  unsigned int ans = 0;
  for (unsigned int t = 0; t < range; ++t) {
    if (tbl[t] > tmax) {
      tmax = tbl[t];
      ans = t;
    }
  }
  free(tbl);
  return ScalarInteger((int)ans + xmin);
}

SEXP Cunique_fmatch(SEXP xx, SEXP ff, SEXP nthreads) {
  if (!isInteger(xx) || !isInteger(ff) || xlength(xx) <= 1) {
    return xx;
  }
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(xx);
  const int * f = INTEGER(ff);
  const int * x = INTEGER(xx);
  int o = 0, M = 0;
  FORLOOP_redsum(o += f[i] >= (i + 1);)

  M = o;

  SEXP ans = PROTECT(allocVector(INTSXP, M));
  int * ansp = INTEGER(ans);
  ansp[0] = x[0]; // by definition
  R_xlen_t j = 0;
  for (R_xlen_t i = 1; i < N; ++i) {
    if (f[i] >= (i + 1)) {
      ++j;
      ansp[j] = x[i];
    }
  }
  UNPROTECT(1);
  return ans;
}



