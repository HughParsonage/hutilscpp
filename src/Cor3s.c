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
  if (o == OP_IN) {
    if (M <= 30) {
      // Rprintf("M <= 30\n");
      for (R_xlen_t i = 0; i < N; ++i) {
        if (ansp[i] == 0) {
          int xi = x[i];
          unsigned char ans_i = 0;
          for (int j = 0; j < M; ++j) {
            if (xi == y[j]) {
              ans_i = 1;
              break;
            }
          }
          ansp[i] = ans_i;
        }
      }
    } else {
      unsigned int fail[1] = {0};
      do_uchar_in_II(ansp, fail,
                     x, N,
                     y, M,
                     nThread,
                     false);
    }
    return;
  }

  if (M == 2) {
    switch(o) {
    case OP_BW: {
      int y0 = y[0];
      int y1 = y[1];
      if (y0 == y1) {
        FORLOOP_ors(==, y0)
        break;
      }
      if (y0 > y1) {
        break;
      }
      // y0 < y1
      if (y0 == 0) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
        for (R_xlen_t i = 0; i < N; ++i) {
          unsigned int xi = x[i];
          ansp[i] |= xi <= y1;
        }
      } else if (y0 > 0) {
        unsigned int u0 = y0;
        unsigned int u1 = y1 - u0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
        for (R_xlen_t i = 0; i < N; ++i) {
          ansp[i] |= (x[i] - u0) <= u1;
        }
      } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
        for (R_xlen_t i = 0; i < N; ++i) {
          int xi = x[i];
          ansp[i] |= xi >= y0;
          ansp[i] |= xi <= y1;
        }
      }
    }


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
        y0 -= (y0 < 0);  // if negative wil be truncated towards zero
      } else {
        if (safety == 2) {
          memset(ansp, 2, N);
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
        if (y0 == 2147483647) {
          return;
        }
        y0 += (y0 < 0);
      }
      break;
    }
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

static void vor2s_DI(unsigned char * ansp,
                      const int o,
                      const double * x,
                      R_xlen_t N,
                      const int * y,
                      R_xlen_t M,
                      int nThread) {
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
        if (y[0] == 1) {
          if (y[1] == 0) {
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
    case OP_BO:
      return;
    case OP_BC:
      return;
    }
  }
  if (o == OP_IN || o == OP_NI) {
    if (M == 0) {
      // No value is in RHS
      if (o == OP_NI) {
        FORLOOP(ansp[i] = 1;)
      }
      return;
    }

    // Reduce to unique elements
    bool tbl[3] = {0};


    for (R_xlen_t j = 0; j < M; ++j) {
      if (y[j] == 0) {
        tbl[0] = true;
      } else if (y[j] == 1) {
        tbl[1] = true;
      } else {
        tbl[2] = true;
      }
    }
    if (tbl[0] && tbl[1] && tbl[2]) {
      // Any value is in RHS
      if (o == OP_IN) {
        FORLOOP(ansp[i] = 1;)
      } else {
        // since "OR FALSE" doesn't modify
        return;
      }
      return;
    }
    // 2-element table
    if (tbl[0] && tbl[1]) {
      if (o == OP_IN) {
        FORLOOP(ansp[i] |= x[i] != NA_LOGICAL;)
      } else {
        FORLOOP(ansp[i] |= x[i] == NA_LOGICAL;)
      }
      return;
    }
    if (tbl[0] && tbl[2]) {
      if (o == OP_IN) {
        FORLOOP(ansp[i] |= x[i] != 1;)
      } else {
        FORLOOP(ansp[i] |= x[i] == 1;)
      }
      return;
    }
    if (tbl[1] && tbl[2]) {
      if (o == OP_IN) {
        FORLOOP(ansp[i] |= x[i] != 0;)
      } else {
        FORLOOP(ansp[i] |= x[i] == 0;)
      }
      return;
    }
    // y must be constant
    if (tbl[0]) {
      if (o == OP_IN) {
        FORLOOP(ansp[i] |= x[i] == 0;)
      } else {
        FORLOOP(ansp[i] |= x[i] != 0;)
      }
    }
    if (tbl[1]) {
      if (o == OP_IN) {
        FORLOOP(ansp[i] |= x[i] == 1;)
      } else {
        FORLOOP(ansp[i] |= x[i] != 1;)
      }
    }
    if (tbl[2]) {
      if (o == OP_IN) {
        FORLOOP(ansp[i] |= x[i] == NA_LOGICAL;)
      } else {
        FORLOOP(ansp[i] |= x[i] != NA_LOGICAL;)
      }
    }
    return;
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

static void vor2s(unsigned char * ansp, const int o,
                  SEXP x, SEXP y, int nThread) {
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
    }
    break;
  }
}

// or
SEXP Cors(SEXP oo1, SEXP xx1, SEXP yy1,
          SEXP oo2, SEXP xx2, SEXP yy2,
          SEXP nthreads) {
  R_xlen_t N = xlength(xx1);
  const bool use2 = oo2 != R_NilValue;
  if (use2 && xlength(xx2) != N) {
    error("`(Cors): xlength(xx1) = %lld`, yet `xlength(xx2) = %lld`.",
          xlength(xx1), xlength(xx2));
  }

  int nThread = as_nThread(nthreads);
  const int temp_o1 = sex2op(oo1);
  if (temp_o1 == OP_IN || temp_o1 == OP_NI) {
    // x %in% a:b    => x %between% c(a, b)
    // x %notin% a:b => x %]between[% c(a, b)
    if (is_seq(yy1)) {
      int y0 = INTEGER(yy1)[0] - (temp_o1 == OP_NI);
      int y2 = INTEGER(yy1)[xlength(yy1) - 1] + (temp_o1 == OP_NI);
      SEXP yyy = PROTECT(allocVector(INTSXP, 2));
      INTEGER(yyy)[0] = y0;
      INTEGER(yyy)[1] = y2;
      UNPROTECT(1);
      return Cors(ScalarInteger(temp_o1 == OP_IN ? OP_BW : OP_BC),
                  xx1, yyy,
                  oo2, xx2, yy2,
                  nthreads);
    }
  }
  const int temp_o2 = sex2op(oo2);
  if (temp_o2 == OP_IN || temp_o2 == OP_NI) {
    // x %in% a:b    => x %between% c(a, b)
    // x %notin% a:b => x %]between[% c(a, b)
    if (is_seq(yy2)) {
      int y0 = INTEGER(yy2)[0] - (temp_o2 == OP_NI);
      int y2 = INTEGER(yy2)[xlength(yy2) - 1] + (temp_o2 == OP_NI);
      SEXP yyy = PROTECT(allocVector(INTSXP, 2));
      INTEGER(yyy)[0] = y0;
      INTEGER(yyy)[1] = y2;
      UNPROTECT(1);
      return Cors(oo1,
                  xx1, yy1,
                  ScalarInteger(temp_o2 == OP_IN ? OP_BW : OP_BC),
                  xx2, yyy,
                  nthreads);
    }
  }

  const int o1 = sex2op(oo1);
  const int o2 = sex2op(oo2);
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * ansp = RAW(ans);

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
    vor2s(ansp, o1, xx1, yy1, nThread);
  }
  if (use2) {
    vor2s(ansp, o2, xx2, yy2, nThread);
  }
  UNPROTECT(1);
  return ans;
}


