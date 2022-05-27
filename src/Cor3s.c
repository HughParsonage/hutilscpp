#include "hutilscpp.h"

#if defined _OPENMP && _OPENMP >= 201511
#define FORLOOP(content)                                                \
_Pragma("omp parallel for num_threads(nThread)")                        \
  for (R_xlen_t i = 0; i < N; ++i) {                                    \
    content                                                             \
  }
#else
#define FORLOOP(content)                                       \
for (R_xlen_t i = 0; i < N; ++i) {                             \
  content                                                      \
}
#endif

#if defined _OPENMP && _OPENMP >= 201511
#define FORLOOP_ors(op, rhs)                                                 \
_Pragma("omp parallel for num_threads(nThread)")                              \
  for (R_xlen_t i = 0; i < N; ++i) {                                          \
    ansp[i] |= x[i] op rhs;                                                   \
  }
#else
#define FORLOOP_ors(op, rhs)                                   \
for (R_xlen_t i = 0; i < N; ++i) {                              \
  ansp[i] |= x[i] op rhs;                                       \
}
#endif


static void vor2s_II(unsigned char * ansp,
                      const int o,
                      const int * x,
                      R_xlen_t N,
                      const int * y,
                      R_xlen_t M,
                      int nThread) {

  if (M == 2 && op_xlen2(o)) {
    const int y0 = y[0];
    const int y1 = y[1];
    if (y0 > y1) {
      return;
    }
    if (y0 == y1) {
      switch(o) {
      case OP_BW:
        FORLOOP_ors(==, y0)
        break;
      case OP_BO:
        break;
      case OP_BC:
        memset(ansp, 1, N);
      }
      return;
    }
    switch(o) {
    case OP_BW: {
      // y0 < y1
      unsigned int uy0 = y0, uy1 = y1;
      unsigned int b = uy1 - uy0;
      if (y0 == 0) {
        FORLOOP({
          unsigned int xi = x[i];
          ansp[i] |= xi <= uy1;
        })
      } else if (y0 > 0) {
        FORLOOP({
          ansp[i] |= (((unsigned int)x[i]) - y0) <= b;
        });
      } else {
        FORLOOP({
          int xi = x[i];
          ansp[i] |= xi >= y0 && xi <= y1;
        })
      }
    }
      break;
    case OP_BO:
      FORLOOP(ansp[i] |= x[i] > y0 && x[i] < y1;)
      break;
    case OP_BC:
      FORLOOP(ansp[i] |= x[i] <= y0 || x[i] >= y1;)
      break;
    }
    return;
  }
  if (M == N) {
    switch(o) {
    case OP_NE:
      FORLOOP_ors(!=, y[i])
      break;
    case OP_EQ:
      FORLOOP_ors(==, y[i])
      break;
    case OP_GT:
      FORLOOP_ors(>, y[i])
      break;
    case OP_LT:
      FORLOOP_ors(<, y[i])
      break;
    case OP_GE:
      FORLOOP_ors(>=, y[i])
      break;
    case OP_LE:
      FORLOOP_ors(<=, y[i])
      break;
    }
  }
  if (M == 1) {
    int y0 = y[0];
    switch(o) {
    case OP_NE:
      FORLOOP_ors(!=, y0)
      break;
    case OP_EQ:
      FORLOOP_ors(==, y0)
      break;
    case OP_GT:
      FORLOOP_ors(>, y0)
      break;
    case OP_LT:
      FORLOOP_ors(<, y0);
      break;
    case OP_GE:
      FORLOOP_ors(>=, y0)
      break;
    case OP_LE:
      FORLOOP_ors(<=, y0)
      break;
    }
  }
}

static void vor2s_ID(unsigned char * ansp,
                     const int o,
                     const int * x,
                     R_xlen_t N,
                     const double * y,
                     R_xlen_t M,
                     int nThread) {
  if (M == 2 && op_xlen2(o)) {
    const double y0 = y[0], y1 = y[1];
    const double yy0 = ISNAN(y0) ? R_NegInf : y0;
    const double yy1 = ISNAN(y1) ? R_PosInf : y1;

    switch(o) {
    case OP_BW:
      uc_betweenidd(ansp, ORAND_OR, x, N, nThread, y0, y1);
      return;
    case OP_BO:
      FORLOOP(ansp[i] |= (x[i] > yy0) && (x[i] < yy1);)
      return;
    case OP_BC:
      FORLOOP(ansp[i] |= (x[i] <= yy0) || (x[i] >= yy1);)
      return;
    }
  }
  if (M == N) {
    switch(o) {
    case OP_NE:
      FORLOOP_ors(!=, y[i])
      break;
    case OP_EQ:
      FORLOOP_ors(==, y[i])
      break;
    case OP_GT:
      FORLOOP_ors(>, y[i])
      break;
    case OP_LT:
      FORLOOP_ors(<, y[i])
      break;
    case OP_GE:
      FORLOOP_ors(>=, y[i])
      break;
    case OP_LE:
      FORLOOP_ors(<=, y[i])
      break;
    }
  }
  if (M == 1) {
    double pre_y0 = y[0];
    int safety = why_dbl_isnt_int(pre_y0);
    int y0 = (safety == DBL_INT || safety == DBL_FRA) ? pre_y0 : 0;
    switch(o) {
    case OP_NE:
      if (safety != DBL_INT) {
        memset(ansp, 1, N);
        // return
      } else {
        int y0 = pre_y0;
        FORLOOP(ansp[i] |= x[i] != y0;)
      }
      return;
    case OP_EQ:
      if (safety != DBL_INT) {
        // can never be true
        // return
      } else {
        int y0 = pre_y0;
        FORLOOP(ansp[i] |= x[i] == y0;)
      }
      return;
    case OP_GE:
      switch(safety) {
      case DBL_INT:
        FORLOOP(ansp[i] |= x[i] >= y0;)
        return;
      case DBL_FRA: {
        y0 += (y0 > 0);
        FORLOOP(ansp[i] |= x[i] >= y0;)
      }
        return;
      case DBL_XHI:
        return;
      case DBL_XLO:
        memset(ansp, 1, N);
        return;
      }
      break; // # nocov
    case OP_GT:
      switch(safety) {
      case DBL_INT:
        FORLOOP(ansp[i] |= x[i] > y0;)
        return;
      case DBL_FRA: {
        y0 += (y0 > 0);
        FORLOOP(ansp[i] |= x[i] >= y0;)
      }
        return;
      case DBL_XHI:
        return;
      case DBL_XLO:
        memset(ansp, 1, N);
        return;
      }
      break; // # nocov
    case OP_LE:
      switch(safety) {
      case DBL_INT:
        FORLOOP(ansp[i] |= x[i] <= y0;)
        return;
      case DBL_FRA: {
        y0 -= (y0 < 0);
        FORLOOP(ansp[i] |= x[i] <= y0;)
      }
        return;
      case DBL_XHI:
        memset(ansp, 1, N);
        return;
      case DBL_XLO:
        return;
      }
      break; // # nocov
    case OP_LT:
      switch(safety) {
      case DBL_INT:
        FORLOOP(ansp[i] |= x[i] < y0;)
        return;
      case DBL_FRA: {
        y0 -= (y0 < 0);
        FORLOOP(ansp[i] |= x[i] <= y0;)
      }
        return;
      case DBL_XHI:
        memset(ansp, 1, N);
        return;
      case DBL_XLO:
        return;
      }
      break; // # nocov
    }
  }
}

static void vor2s_DI(unsigned char * ansp,
                      const int o,
                      const double * x,
                      R_xlen_t N,
                      const int * y,
                      R_xlen_t M,
                      int nThread) {
  if (M == 2 && op_xlen2(o)) {
    const double y0 = y[0];
    const double y1 = y[1];
    switch(o) {
    case OP_BW:
      FORLOOP(ansp[i] |= x[i] >= y0 && x[i] <= y1;)
      break;
    case OP_BO:
      FORLOOP(ansp[i] |= x[i] > y0 && x[i] < y1;)
      break;
    case OP_BC:
      FORLOOP(ansp[i] |= x[i] <= y0 || x[i] >= y1;)
      break;
    }
    return;
  }
  if (M == N) {
    switch(o) {
    case OP_NE:
      FORLOOP_ors(!=, y[i])
      break;
    case OP_EQ:
      FORLOOP_ors(==, y[i])
      break;
    case OP_GT:
      FORLOOP_ors(>, y[i])
      break;
    case OP_LT:
      FORLOOP_ors(<, y[i])
      break;
    case OP_GE:
      FORLOOP_ors(>=, y[i])
      break;
    case OP_LE:
      FORLOOP_ors(<=, y[i])
      break;
    }
  }
  if (M == 1) {
    int y0 = y[0];
    switch(o) {
    case OP_NE:
      FORLOOP_ors(!=, y0)
      break;
    case OP_EQ:
      FORLOOP_ors(==, y0)
      break;
    case OP_GT:
      FORLOOP_ors(>, y0)
      break;
    case OP_LT:
      FORLOOP_ors(<, y0)
      break;
    case OP_GE:
      FORLOOP_ors(>=, y0)
      break;
    case OP_LE:
      FORLOOP_ors(<=, y0)
      break;
    }
  }
}

static void vor2s_DD(unsigned char * ansp,
                      const int o,
                      const double * x,
                      R_xlen_t N,
                      const double * y,
                      R_xlen_t M,
                      int nThread) {
  if (M == 2 && op_xlen2(o)) {
    const double y0 = y[0], y1 = y[1];
    switch(o) {
    case OP_BW:
      FORLOOP(ansp[i] |= (x[i] >= y0) && (x[i] <= y1);)
      break;
    case OP_BO:
      FORLOOP(ansp[i] |= (x[i] > y0) && (x[i] < y1);)
      break;
    case OP_BC:
      FORLOOP(ansp[i] |= (x[i] <= y0) || (x[i] >= y1);)
      break;
    }
    return;
  }
  if (M == N) {
    switch(o) {
    case OP_NE:
      FORLOOP_ors(!=, y[i])
      break;
    case OP_EQ:
      FORLOOP_ors(==, y[i])
      break;
    case OP_GT:
      FORLOOP_ors(>, y[i])
      break;
    case OP_LT:
      FORLOOP_ors(<, y[i])
      break;
    case OP_GE:
      FORLOOP_ors(>=, y[i])
      break;
    case OP_LE:
      FORLOOP_ors(<=, y[i])
      break;
    }
  }
  if (M == 1) {
    double y0 = y[0];
    switch(o) {
    case OP_NE:
      FORLOOP_ors(!=, y0)
      break;
    case OP_EQ:
      FORLOOP_ors(==, y0)
      break;
    case OP_GT:
      FORLOOP_ors(>, y0)
      break;
    case OP_LT:
      FORLOOP_ors(<, y0)
      break;
    case OP_GE:
      FORLOOP_ors(>=, y0)
      break;
    case OP_LE:
      FORLOOP_ors(<=, y0)
      break;
    }
  }
}

static void vo2s_LL(unsigned char * ansp, const int o,
                    const int * x, R_xlen_t N,
                    const int * y, R_xlen_t M,
                    int nThread) {
  if (M == 1) {
    const int y0 = y[0];
    switch(o) {
    case OP_NI:
    case OP_NE:
      FORLOOP(ansp[i] |= x[i] != y0;)
      break;
    case OP_IN:
    case OP_EQ:
      FORLOOP(ansp[i] |= x[i] == y0;)
      break;
    case OP_GE:
      FORLOOP(ansp[i] |= x[i] >= y0;)
      break;
    case OP_LE:
      FORLOOP(ansp[i] |= x[i] <= y0;)
      break;
    case OP_GT:
      FORLOOP(ansp[i] |= x[i] > y0;)
      break;
    case OP_LT:
      FORLOOP(ansp[i] |= x[i] < y0;)
      break;
    }
    return;
  }
  if (M == 2) {
    switch(o) {
    case OP_BW:
      if (y[0] == 0 && y[1] == 1) {
        FORLOOP(ansp[i] = 1;)
        return;
      } else {
        int y00 = y[0] == NA_LOGICAL ? 0 : y[0];
        int y01 = y[1] == NA_LOGICAL ? 1 : y[1];
        if (y01 < y00) {
          return;
        }
        FORLOOP(ansp[i] |= x[i] == y01 || x[i] == y01;)
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
          FORLOOP(ansp[i] |= x[i] == 1;)
            return;
        }
        if (y[1] == 1) {
          FORLOOP(ansp[i] = 1;)
        }
        return;
      }
      break;
      // # nocov end
    case OP_BO:
      return;
    case OP_BC:
      return;
    }
  }

  if (N == M) {
    switch(o) {
    case OP_NE:
      FORLOOP(ansp[i] |= x[i] != y[i];)
      break;
    case OP_EQ:
      FORLOOP(ansp[i] |= x[i] == y[i];)
      break;
    case OP_GE:
      FORLOOP(ansp[i] |= x[i] >= y[i];)
      break;
    case OP_LE:
      FORLOOP(ansp[i] |= x[i] <= y[i];)
      break;
    case OP_GT:
      FORLOOP(ansp[i] |= x[i] > y[i];)
      break;
    case OP_LT:
      FORLOOP(ansp[i] |= x[i] < y[i];)
      break;
    }
  }
}

static void vo2s_L(unsigned char * ansp, const int o,
                   const int * x, R_xlen_t N,
                   int nThread) {
  if (o == OP_EQ) {
    FORLOOP(ansp[i] |= x[i] == 1;)
  } else {
    FORLOOP(ansp[i] |= x[i] != 1;)
  }
}

static void vor2_SeqS1(unsigned char * ansp, const SEXP * xp, R_xlen_t N, const char * y, const int ny) {
  for (R_xlen_t i = 0; i < N; ++i) {
    if (ansp[i]) {
      continue;
    }
    int nxi = length(xp[i]);
    if (nxi != ny) {
      continue;
    }
    const char * xi = CHAR(xp[i]);
    ansp[i] = string_equal(xi, y);
  }
}

static void vor2_SneqS1(unsigned char * ansp, const SEXP * xp, R_xlen_t N, const char * y, const int ny) {
  for (R_xlen_t i = 0; i < N; ++i) {
    if (ansp[i]) {
      continue;
    }
    int nxi = length(xp[i]);
    if (nxi != ny) {
      ansp[i] = 1;
      continue;
    }
    const char * xi = CHAR(xp[i]);
    ansp[i] = !string_equal(xi, y);
  }
}

static void vor2_SeqS(unsigned char * ansp, const SEXP * xp, R_xlen_t N, const SEXP * yp) {
  for (R_xlen_t i = 0; i < N; ++i) {
    if (ansp[i]) {
      continue;
    }
    ansp[i] = string_equal(CHAR(xp[i]), CHAR(yp[i]));
  }
}

static void vor2_SneqS(unsigned char * ansp, const SEXP * xp, R_xlen_t N, const SEXP * yp) {
  for (R_xlen_t i = 0; i < N; ++i) {
    if (ansp[i]) {
      continue;
    }
    ansp[i] = !string_equal(CHAR(xp[i]), CHAR(yp[i]));
  }
}

static void vor2_SS(unsigned char * ansp, const int o,
                     const SEXP * x, R_xlen_t N,
                     const SEXP * y, R_xlen_t M) {
  if (M == 1) {
    if (o == OP_EQ) {
      vor2_SeqS1(ansp, x, N, CHAR(y[0]), length(y[0]));
    } else {
      vor2_SneqS1(ansp, x, N, CHAR(y[0]), length(y[0]));
    }
  } else {
    if (o == OP_EQ) {
      vor2_SeqS(ansp, x, N, y);
    } else {
      vor2_SneqS(ansp, x, N, y);
    }
  }
}

static void vor2s(unsigned char * ansp, const int o,
                  SEXP x, SEXP y, int nThread,
                  int * err) {
  R_xlen_t N = xlength(x);
  R_xlen_t M = xlength(y);

  switch(TYPEOF(x)) {
  case LGLSXP:
    switch(TYPEOF(y)) {
    case LGLSXP:
      vo2s_LL(ansp, o, LOGICAL(x), N, LOGICAL(y), M, nThread);
      break;
    default:
      vo2s_L(ansp, o, LOGICAL(x), N, nThread);
    }
    break;
  case INTSXP:
    switch(TYPEOF(y)) {
    case INTSXP:
      vor2s_II(ansp, o, INTEGER(x), N, INTEGER(y), M, nThread);
      break;
    case REALSXP:
      vor2s_ID(ansp, o, INTEGER(x), N, REAL(y), M, nThread);
      break;
    default:
      *err = OR3__UNSUPPORTED_TYPEY;
    }
    break;
  case REALSXP:
    switch(TYPEOF(y)) {
    case INTSXP:
      vor2s_DI(ansp, o, REAL(x), N, INTEGER(y), M, nThread);
      break;
    case REALSXP:
      vor2s_DD(ansp, o, REAL(x), N, REAL(y), M, nThread);
      break;
    default:
      *err = OR3__UNSUPPORTED_TYPEY;
    }
    break;
  case STRSXP:
    if (TYPEOF(y) == STRSXP && (o == OP_EQ || o == OP_NE)) {
      // only support == and !=
      vor2_SS(ansp, o, STRING_PTR(x), N, STRING_PTR(y), M);
    } else {
      *err = OR3__UNSUPPORTED_TYPEY;
    }
    break;
  default:
    *err = OR3__UNSUPPORTED_TYPEX;
  }
}

// or
SEXP Cors(SEXP oo1, SEXP xx1, SEXP yy1,
          SEXP oo2, SEXP xx2, SEXP yy2,
          SEXP nthreads) {
  R_xlen_t N = xlength(xx1);
  const bool use2 = oo2 != R_NilValue;
  if (use2 && xlength(xx2) != N) {
    // # nocov start
    error("`(Cors): xlength(xx1) = %lld`, yet `xlength(xx2) = %lld`.",
          xlength(xx1), xlength(xx2));
    // # nocov end
  }

  int nThread = as_nThread(nthreads);
  const int o1 = sex2op(oo1);
  const int o2 = sex2op(oo2);
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * ansp = RAW(ans);
  int err[1] = {0};

  if (yy1 == R_NilValue && isLogical(xx1)) {
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
  } else {
    FORLOOP(
      ansp[i] = 0;
    )
    vor2s(ansp, o1, xx1, yy1, nThread, err);
  }
  if (use2) {
    vor2s(ansp, o2, xx2, yy2, nThread, err);
  }
  UNPROTECT(1);
  if (err[0]) {
    REprintf("Unsupported type");
    return R_NilValue;
  }
  return ans;
}


