#include "hutilscpp.h"

// # nocov start
SEXP C_and_raw(SEXP x, SEXP y, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(x);

  if (N == 0) {
    return x;
  }
  if (xlength(y) == 0) {
    return x;
  }
  if (xlength(y) != 1 && xlength(y) != N) {
    warning("Internal error(.and_raw): y had bad length, so x will be returned.");
    return x;
  }
  if (isntRaw(y) && !isLogical(y)) {
    return y;
  }
  if (xlength(y) == 1) {
    const unsigned char y0 = isntRaw(y) ? (asLogical(y) == 1) : RAW(y)[0];
    if (y0 == 1) {
      return x;
    }
    switch(TYPEOF(x)) {
    case RAWSXP: {
      unsigned char * xp = RAW(x);
      FORLOOP(xp[i] = 0;)
    }
      break;
    case LGLSXP: {
      int * xp = LOGICAL(x);
      FORLOOP(xp[i] = 0;)
    }
      break;
    }
    return x;
  }



  switch(TYPEOF(x)) {
  case LGLSXP: {
    int * xp = LOGICAL(x);
    switch(TYPEOF(y)) {
    case LGLSXP: {
      const int * yp = LOGICAL(y);
      FORLOOP(xp[i] &= yp[i];)
    }
      break;
    case RAWSXP: {
      const unsigned char * yp = RAW(y);
      FORLOOP(xp[i] &= yp[i];)
    }
      break;
    }
  }
    break;

  case RAWSXP: {
    unsigned char * xp = RAW(x);
    switch(TYPEOF(y)) {
    case LGLSXP: {
      const int * yp = LOGICAL(y);
      FORLOOP(xp[i] &= yp[i];)
    }
      break;
    case RAWSXP: {
      const unsigned char * yp = RAW(y);
      FORLOOP(xp[i] &= yp[i];)
    }
      break;
    }
  }
    break;
  }

  return x;
}


SEXP C_or_raw(SEXP x, SEXP y, SEXP nthreads) {
  int nThread = asInteger(nthreads);
  R_xlen_t N = xlength(x);
  if (N == 0) {
    return x;
  }
  if (xlength(y) == 0) {
    return x;
  }
  if (xlength(y) != 1 && xlength(y) != N) {
    warning("Internal error(.and_raw): y had bad length, so x will be returned.");
    return x;
  }
  if (isntRaw(y) && !isLogical(y)) {
    return y;
  }
  if (xlength(y) == 1) {
    const unsigned char y0 = isntRaw(y) ? (asLogical(y) == 1) : RAW(y)[0];
    if (y0 == 0) {
      return x;
    }
    switch(TYPEOF(x)) {
    case RAWSXP: {
      unsigned char * xp = RAW(x);
      FORLOOP(xp[i] = 1;)
    }
      break;
    case LGLSXP: {
      int * xp = LOGICAL(x);
      FORLOOP(xp[i] = 1;)
    }
      break;
    }
    return x;
  }


  switch(TYPEOF(x)) {
  case LGLSXP: {
    int * xp = LOGICAL(x);
    switch(TYPEOF(y)) {
    case LGLSXP: {
      const int * yp = LOGICAL(y);
      FORLOOP(xp[i] |= yp[i];)
    }
      break;
    case RAWSXP: {
      const unsigned char * yp = RAW(y);
      FORLOOP(xp[i] |= yp[i];)
    }
      break;
    }
  }
    break;
  case RAWSXP: {
    unsigned char * xp = RAW(x);
    switch(TYPEOF(y)) {
    case LGLSXP: {
      const int * yp = LOGICAL(y);
      FORLOOP(xp[i] |= yp[i];)
    }
      break;
    case RAWSXP: {
      const unsigned char * yp = RAW(y);
      FORLOOP(xp[i] |= yp[i];)
    }
      break;
    }
  }
    break;
  }

  return x;
}
// # nocov end

static void vand2s_II(unsigned char * ansp,
                      const int o,
                      const int * x,
                      R_xlen_t N,
                      const int * y,
                      R_xlen_t M,
                      int nThread) {
  if (M == 2) {
    int y0 = y[0];
    int y1 = y[1];
    if (y0 > y1) {
      // memset(ansp, 0, N);
      FORLOOP(ansp[i] = 0;)
      return;
    }
    switch(o) {
    case OP_BW: {
      if (y0 == y1) {
      FORLOOP_ands(==, y0)
      break;
    }
      // y0 < y1
      if (y0 == 0) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
        for (R_xlen_t i = 0; i < N; ++i) {
          unsigned int xi = x[i];
          ansp[i] &= xi <= y1;
        }
      } else if (y0 > 0) {
        unsigned int u0 = y0;
        unsigned int u1 = y1 - u0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] &= (x[i] - u0) <= u1;
        }
      } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
        for (R_xlen_t i = 0; i < N; ++i) {
          int xi = x[i];
          ansp[i] &= xi >= y0;
          ansp[i] &= xi <= y1;
        }
      }
    }
      break;
    case OP_BO:
      // + 1u because y0 maybe INT_MAX
      FORLOOP(ansp[i] &= betweeniiuu(x[i], y0 + 1u, y1 - 1u);)
      break;
    case OP_BC:
      FORLOOP(ansp[i] &= !betweeniiuu(x[i], y0 + 1u, y1 - 1u);)
      break;

    }
    return;
  }
  if (M == N) {
    switch(o) {
    case OP_NE:
      FORLOOP_ands(!=, y[i])
      break;
    case OP_EQ:
      FORLOOP_ands(==, y[i])
      break;
    case OP_GT:
      FORLOOP_ands(>, y[i])
      break;
    case OP_LT:
      FORLOOP_ands(<, y[i])
      break;
    case OP_GE:
      FORLOOP_ands(>=, y[i])
      break;
    case OP_LE:
      FORLOOP_ands(<=, y[i])
      break;
    }
  }
  if (M == 1) {
    int y0 = y[0];
    switch(o) {
    case OP_NE:
      FORLOOP_ands(!=, y0)
      break;
    case OP_EQ:
      FORLOOP_ands(==, y0)
      break;
    case OP_GT:
      FORLOOP_ands(>, y0)
      break;
    case OP_LT:
      FORLOOP_ands(<, y0);
      break;
    case OP_GE:
      FORLOOP_ands(>=, y0)
      break;
    case OP_LE:
      FORLOOP_ands(<=, y0)
      break;
    }
  }
}

static void vand2s_ID(unsigned char * ansp,
                      const int o,
                      const int * x,
                      R_xlen_t N,
                      const double * y,
                      R_xlen_t M,
                      int nThread) {
  if (M == 2 && op_xlen2(o)) {
    bool y0_NAN = ISNAN(y[0]);
    bool y1_NAN = ISNAN(y[1]);

    if (o == OP_BC) {
      if (y0_NAN && y1_NAN) {
        // Unusual x %between% c(NA, NA)
        return; // # nocov
      }
      if (y0_NAN) {
        FORLOOP(ansp[i] &= x[i] >= y[1];)
        return;
      }
      if (y1_NAN) {
        FORLOOP(ansp[i] &= x[i] <= y[0];)
        return;
      }
    }
    double pre_y0 = y0_NAN ? R_NegInf : y[0];
    double pre_y1 = y1_NAN ? R_PosInf : y[1];
    if (pre_y0 > pre_y1) {
      FORLOOP(ansp[i] = 0;)
      return;
    }
    switch(o) {
    case OP_BW:
      uc_betweenidd(ansp, ORAND_AND, x, N, nThread, pre_y0, pre_y1);
      break;
    case OP_BO:
      FORLOOP(ansp[i] &= (x[i] > pre_y0) && (x[i] < pre_y1);)
      break;
    case OP_BC:
      FORLOOP(ansp[i] &= (x[i] <= pre_y0) || (x[i] >= pre_y1);)
      break;
    }
    return;
  }
  if (M == N) {
    switch(o) {
    case OP_NE:
      FORLOOP_ands(!=, y[i])
      break;
    case OP_EQ:
      FORLOOP_ands(==, y[i])
      break;
    case OP_GT:
      FORLOOP_ands(>, y[i])
      break;
    case OP_LT:
      FORLOOP_ands(<, y[i])
      break;
    case OP_GE:
      FORLOOP_ands(>=, y[i])
      break;
    case OP_LE:
      FORLOOP_ands(<=, y[i])
      break;
    }
  }
  if (M == 1) {
    double pre_y0 = y[0];
    int safety = dbl_is_int(pre_y0);
    int y0 = dbl2int(pre_y0); // tempo
    switch(o) {
    case OP_NE:
      if (safety == 0) {
        return;
      }
      break;
    case OP_EQ:
      if (safety == 0) {
        memset(ansp, 0, N);
        return;
      }
      break;
    case OP_GE:
    case OP_GT:
      if (safety == 0) {
        if (pre_y0 > INT_MAX) {
          memset(ansp, 0, N);
          return;
        }
        if (pre_y0 <= -2147483647) {
          return; // always true
        }
        y0 = (int)pre_y0;
        y0 -= (pre_y0 < 0);  // if negative wil be truncated towards zero
      } else {
        if (safety == 2) {
          memset(ansp, 0, N);
          return;
        }
      }
      break;
    case OP_LE:
    case OP_LT:
      if (safety == 0) {
        if (pre_y0 < -2147483647) {
          memset(ansp, 0, N);
          return;
        }
        if (pre_y0 >= 2147483647) {
          return;
        }
        y0 = (int)pre_y0;
        y0 += (y0 < 0);
      }
      break;
    }
    switch(o) {
    case OP_NE:
      FORLOOP_ands(!=, y0)
      break;
    case OP_EQ:
      FORLOOP_ands(==, y0)
      break;
    case OP_GT:
      FORLOOP_ands(>, y0)
      break;
    case OP_LT:
      FORLOOP_ands(<, y0)
      break;
    case OP_GE:
      FORLOOP_ands(>=, y0)
      break;
    case OP_LE:
      FORLOOP_ands(<=, y0)
      break;
    }
  }
}

static void vand2s_DI(unsigned char * ansp,
                      const int o,
                      const double * x,
                      R_xlen_t N,
                      const int * y,
                      R_xlen_t M,
                      int nThread) {
  if (M == 2 && op_xlen2(o)) {
    int y0 = y[0];
    int y1 = y[1];
    switch(o) {
    case OP_BW:
      FORLOOP(ansp[i] = x[i] >= y0 && x[i] <= y1;)
      return;
    case OP_BO:
      FORLOOP(ansp[i] = x[i] > y0 && x[i] < y1;)
      return;
    case OP_BC:
      FORLOOP(ansp[i] = x[i] <= y0 || x[i] >= y1;)
      return;
    }
  }
  if (M == N) {
    switch(o) {
    case OP_NE:
      FORLOOP_ands(!=, y[i])
      break;
    case OP_EQ:
      FORLOOP_ands(==, y[i])
      break;
    case OP_GT:
      FORLOOP_ands(>, y[i])
      break;
    case OP_LT:
      FORLOOP_ands(<, y[i])
      break;
    case OP_GE:
      FORLOOP_ands(>=, y[i])
      break;
    case OP_LE:
      FORLOOP_ands(<=, y[i])
      break;
    }
  }
  if (M == 1) {
    int y0 = y[0];
    switch(o) {
    case OP_NE:
      FORLOOP_ands(!=, y0)
      break;
    case OP_EQ:
      FORLOOP_ands(==, y0)
      break;
    case OP_GT:
      FORLOOP_ands(>, y0)
      break;
    case OP_LT:
      FORLOOP_ands(<, y0)
      break;
    case OP_GE:
      FORLOOP_ands(>=, y0)
      break;
    case OP_LE:
      FORLOOP_ands(<=, y0)
      break;
    }
  }
}

static void vand2s_DD(unsigned char * ansp,
                      const int o,
                      const double * x,
                      R_xlen_t N,
                      const double * y,
                      R_xlen_t M,
                      int nThread) {
  if (M == 2) {
    switch(o) {
    case OP_BW:
      FORLOOP(ansp[i] &= x[i] >= y[0] && x[i] <= y[1];);
      break;
    case OP_BO:
      FORLOOP(ansp[i] &= x[i] > y[0] && x[i] < y[1];);
      break;
    case OP_BC:
      FORLOOP(ansp[i] &= x[i] <= y[0] || x[i] >= y[1];);
      break;
    }
    if (o == OP_BW || o == OP_BO || o == OP_BC) {
      return;
    }

  }
  if (M == N) {
    switch(o) {
    case OP_NE:
      FORLOOP_ands(!=, y[i])
      break;
    case OP_EQ:
      FORLOOP_ands(==, y[i])
      break;
    case OP_GT:
      FORLOOP_ands(>, y[i])
      break;
    case OP_LT:
      FORLOOP_ands(<, y[i])
      break;
    case OP_GE:
      FORLOOP_ands(>=, y[i])
      break;
    case OP_LE:
      FORLOOP_ands(<=, y[i])
      break;
    }
  }
  if (M == 1) {
    double y0 = y[0];
    switch(o) {
    case OP_NE:
      FORLOOP_ands(!=, y0)
      break;
    case OP_EQ:
      FORLOOP_ands(==, y0)
      break;
    case OP_GT:
      FORLOOP_ands(>, y0)
      break;
    case OP_LT:
      FORLOOP_ands(<, y0)
      break;
    case OP_GE:
      FORLOOP_ands(>=, y0)
      break;
    case OP_LE:
      FORLOOP_ands(<=, y0)
      break;
    }
  }
}

static void vand2s_LL(unsigned char * ansp, const int o,
                      const int * x, R_xlen_t N,
                      const int * y, R_xlen_t M,
                      int nThread) {
  if (M == 1) {
    const int y0 = y[0];
    switch(o) {
    case OP_NI:
    case OP_NE:
      FORLOOP(ansp[i] &= x[i] != y0;)
      break;
    case OP_IN:
    case OP_EQ:
      FORLOOP(ansp[i] &= x[i] == y0;)
      break;
    case OP_GE:
      FORLOOP(ansp[i] &= x[i] >= y0;)
      break;
    case OP_LE:
      FORLOOP(ansp[i] &= x[i] <= y0;)
      break;
    case OP_GT:
      FORLOOP(ansp[i] &= x[i] > y0;)
      break;
    case OP_LT:
      FORLOOP(ansp[i] &= x[i] < y0;)
      break;
    }
    return;
  }
  if (M == 2) {
    switch(o) {
    case OP_BW:
      if (y[0] == 0 && y[1] == 1) {
        return;
      } else {
        FORLOOP(ansp[i] &= x[i] >= y[0] && x[i] <= y[1];)
        return;
      }
      break;
      // # nocov start
    case OP_WB:
      if (y[0] == 0 && y[1] == 1) {
        return;
      } else {
        if (y[0] == 1) {
          if (y[1] == 0) {
            FORLOOP(ansp[i] = 1;)
            return;
          }
          FORLOOP(ansp[i] &= x[i] == 1;)
            return;
        }
        if (y[1] == 1) {
          FORLOOP(ansp[i] = 1;)
        }
        return;
      }
      break;
    case OP_BO:
      return;
    case OP_BC:
      return;
    }
    // # nocov end
  }


  if (N == M) {
    switch(o) {
    case OP_NE:
      FORLOOP(ansp[i] &= x[i] != y[i];)
      break;
    case OP_EQ:
      FORLOOP(ansp[i] &= x[i] == y[i];)
      break;
    case OP_GE:
      FORLOOP(ansp[i] &= x[i] >= y[i];)
      break;
    case OP_LE:
      FORLOOP(ansp[i] &= x[i] <= y[i];)
      break;
    case OP_GT:
      FORLOOP(ansp[i] &= x[i] > y[i];)
      break;
    case OP_LT:
      FORLOOP(ansp[i] &= x[i] < y[i];)
      break;
    }
  }
}

static void vand2s_L(unsigned char * ansp, const int o,
                     const int * x, R_xlen_t N,
                     int nThread) {

  if (o == OP_EQ) {
    FORLOOP(ansp[i] &= x[i] == 1;)
  } else {
    FORLOOP(ansp[i] &= x[i] != 1;)
  }
}

static void vand2s_R(unsigned char * ansp, const int o,
                     const unsigned char * x, R_xlen_t N,
                     int nThread) {
  if (o == OP_EQ) {
    FORLOOP(ansp[i] &= x[i];)
  } else {
    FORLOOP(ansp[i] ^= x[i];) // # nocov
  }
}

static void vand2s(unsigned char * ansp, const int o,
                   SEXP x, SEXP y, int nThread) {
  R_xlen_t N = xlength(x);
  R_xlen_t M = xlength(y);

  switch(TYPEOF(x)) {
  case LGLSXP:
    switch(TYPEOF(y)) {
    case LGLSXP:
      vand2s_LL(ansp, o, LOGICAL(x), N, LOGICAL(y), M, nThread);
      break;
    default:
      vand2s_L(ansp, o, LOGICAL(x), N, nThread);
    }
    break;

  case INTSXP:
    switch(TYPEOF(y)) {
    case INTSXP:
      vand2s_II(ansp, o, INTEGER(x), N, INTEGER(y), M, nThread);
      break;
    case REALSXP:
      vand2s_ID(ansp, o, INTEGER(x), N, REAL(y), M, nThread);
      break;
    }
    break;
  case REALSXP:
    switch(TYPEOF(y)) {
    case INTSXP:
      vand2s_DI(ansp, o, REAL(x), N, INTEGER(y), M, nThread);
      break;
    case REALSXP:
      vand2s_DD(ansp, o, REAL(x), N, REAL(y), M, nThread);
      break;
    }
    break;
  case RAWSXP:
    vand2s_R(ansp, o, RAW(x), N, nThread);
  }
}


SEXP Cands(SEXP oo1, SEXP xx1, SEXP yy1,
           SEXP oo2, SEXP xx2, SEXP yy2,
           SEXP nthreads) {
  R_xlen_t N = xlength(xx1);
  const bool use2 = oo2 != R_NilValue;
  // # nocov start
  if (use2 && xlength(xx2) != N) {
    error("`(Cands1): xlength(xx1) = %lld`, yet `xlength(xx2) = %lld`. type '%s'",
          xlength(xx1), xlength(xx2), type2char(TYPEOF(xx2)));
  }
  // # nocov end
  int nThread = as_nThread(nthreads);

  const int o1 = sex2op(oo1);
  const int o2 = sex2op(oo2);
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * ansp = RAW(ans);
  if (TYPEOF(yy1) == NILSXP) {
    switch(TYPEOF(xx1)) {
    case LGLSXP: {
    const int * xx1p = LOGICAL(xx1);
    if (o1 == OP_NE) {
      FORLOOP(
        ansp[i] = xx1p[i] != 1;
      )
    } else {
      FORLOOP(
        ansp[i] = xx1p[i] != 0;
      )
    }
  }
    break;
  case RAWSXP: {
    const unsigned char * xx1p = RAW(xx1);
    if (o1 == OP_NE) {
      FORLOOP(
        ansp[i] = xx1p[i] != 1;
      )
    } else {
      FORLOOP(
        ansp[i] = xx1p[i] != 0;
      )
    }
  }
    break;
    // # nocov start
  default: {
    error("Internal error(Cand3s): unsupported xx1 with NILSXP yy1;");
  }
  }
    // # nocov end
  } else {
    FORLOOP({
      ansp[i] = 1;
    })
    vand2s(ansp, o1, xx1, yy1, nThread);
  }
  if (use2) {
    vand2s(ansp, o2, xx2, yy2, nThread);
  }
  UNPROTECT(1);
  return ans;
}




SEXP C_which_raw(SEXP X, SEXP nthreads) {
#if defined _OPENMP
  int nThread = as_nThread(nthreads);
#endif
  R_xlen_t N = xlength(X);
  const unsigned char * xp = RAW(X);
  R_xlen_t o = 0, last = 0;
  if (N <= INT_MAX) {
    FORLOOP_redsum(o += xp[i] != 0;)
  } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : o) reduction(max : last)
#endif
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xp[i] != 0) {
        o++;
        last = i + 1;
      }
    }
  }
  if (last < INT_MAX) {
    SEXP ans = PROTECT(allocVector(INTSXP, o));
    int * restrict ansp = INTEGER(ans);
    int j = 0;
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[j] = i + 1;
      j += (bool)xp[i];
      if (j >= o) break;
    }
    UNPROTECT(1);
    return ans;
  }

  SEXP ans = PROTECT(allocVector(REALSXP, o));
  double * restrict ansp = REAL(ans);
  R_xlen_t j = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[j] = i + 1;
    j += (bool)xp[i];
  }
  UNPROTECT(1);
  return ans;

}

