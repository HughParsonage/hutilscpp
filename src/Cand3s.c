
#include "hutilscpp.h"

#if defined _OPENMP && _OPENMP >= 201511
#define FORLOOP                                                \
_Pragma("omp parallel for num_threads(nThread)")               \
  for (R_xlen_t i = 0; i < N; ++i)
#else
#define FORLOOP for (R_xlen_t i = 0; i < N; ++i)
#endif

#if defined _OPENMP && _OPENMP >= 201511
#define FORLOOP_ands(op, rhs)                                                \
_Pragma("omp parallel for num_threads(nThread)")                             \
  for (R_xlen_t i = 0; i < N; ++i) {                                         \
    ansp[i] = x[i] op rhs;                                                   \
  }
#else
#define FORLOOP_ands(op, rhs)                                  \
for (R_xlen_t i = 0; i < N; ++i) {                             \
  ansp[i] = x[i] op rhs;                                       \
}
#endif


static SEXP ieqi_1(const int * x, R_xlen_t N, int y, int nThread) {
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * ansp = RAW(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = x[i] == y;
  }
  UNPROTECT(1);
  return ans;
}

static SEXP eq(SEXP xx, SEXP yy, int nThread) {
  switch(TYPEOF(xx)) {
  case INTSXP:
    switch(TYPEOF(yy)) {
    case INTSXP:
      if (xlength(yy) == 1) {
        return ieqi_1(INTEGER(xx), xlength(xx), asInteger(yy), nThread);
      } else {
        return ieqi_1(INTEGER(xx), xlength(xx), asInteger(yy), nThread);
      }
    }
  }
}

static void vand2s_Inei(unsigned char * ansp,
                        const int * x,
                        R_xlen_t N,
                        const int y0,
                        int nThread) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] &= x[i] != y0;
  }
}

static void vand2s_IneI(unsigned char * ansp,
                        const int * x,
                        R_xlen_t N,
                        const int * y,
                        int nThread) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] &= x[i] != y[i];
  }
}
static void vand2s_IneD(unsigned char * ansp,
                        const int * x,
                        R_xlen_t N,
                        const double * y,
                        int nThread) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] &= x[i] != y[i];
  }
}

static void vand2s_Dnei(unsigned char * ansp,
                        const double * x,
                        R_xlen_t N,
                        const int y0,
                        int nThread) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] &= x[i] != y0;
  }
}

static void vand2s_DneI(unsigned char * ansp,
                        const double * x,
                        R_xlen_t N,
                        const int * y,
                        int nThread) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] &= x[i] != y[i];
  }
}

static void vand2s_Dned(unsigned char * ansp,
                        const double * x,
                        R_xlen_t N,
                        const double y0,
                        int nThread) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] &= x[i] != y0;
  }
}

static void vand2s_DneD(unsigned char * ansp,
                        const double * x,
                        R_xlen_t N,
                        const double * y,
                        int nThread) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] &= x[i] != y[i];
  }
}

static void vand2s_not(unsigned char * ansp,
                       const int * x,
                       R_xlen_t N,
                       int nThread) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    if (x[i]) {
      ansp[i] = 0;
    }
  }
}

static void vand2s_yes(unsigned char * ansp,
                       const int * x,
                       R_xlen_t N,
                       int nThread) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    if (!x[i]) {
      ansp[i] = 0;
    }
  }
}

static void vand2s_Ieqi(unsigned char * ansp,
                        const int * x,
                        R_xlen_t N,
                        const int y,
                        int nThread) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] &= x[i] == y;
  }
}

static void vand2s_IeqI(unsigned char * ansp,
                        const int * x,
                        R_xlen_t N,
                        const int * y,
                        int nThread) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] &= x[i] == y[i];
  }
}

static void vand2s_Deqi(unsigned char * ansp,
                        const double * x,
                        R_xlen_t N,
                        const int y,
                        int nThread) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] &= x[i] == y;
  }
}

static void vand2s_DeqI(unsigned char * ansp,
                        const double * x,
                        R_xlen_t N,
                        const int * y,
                        int nThread) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] &= x[i] == y[i];
  }
}

static void vand2s_II(unsigned char * ansp,
                      const int o,
                      const int * x,
                      R_xlen_t N,
                      const int * y,
                      R_xlen_t M,
                      int nThread) {
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

static void vand2s_ID(unsigned char * ansp,
                      const int o,
                      const int * x,
                      R_xlen_t N,
                      const double * y,
                      R_xlen_t M,
                      int nThread) {
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

static void vand2s(unsigned char * ansp, const int o,
                   SEXP x, SEXP y, int nThread) {
  R_xlen_t N = xlength(x);
  R_xlen_t M = xlength(y);
  switch(TYPEOF(x)) {
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
  }
}



SEXP Cands(SEXP oo1, SEXP xx1, SEXP yy1,
           SEXP oo2, SEXP xx2, SEXP yy2,
           SEXP nthreads) {
  R_xlen_t N = xlength(xx1);

  int nThread = as_nThread(nthreads);
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * restrict ansp = RAW(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = 1;
  }
  const int o1 = sex2op(oo1);
  const int o2 = sex2op(oo2);
  vand2s(ansp, o1, xx1, yy1, nThread);
  vand2s(ansp, o2, xx2, yy2, nThread);
  UNPROTECT(1);
  return ans;
}




SEXP Cand3s_par(SEXP xx, SEXP oxx, SEXP x11, SEXP x22,
                SEXP yy, SEXP oyy, SEXP y11, SEXP y22,
                SEXP zz, SEXP ozz, SEXP z11, SEXP z22,
                SEXP AA,
                SEXP BB,
                SEXP CC,
                SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  const int * x = INTEGER(xx);
  const int * y = INTEGER(yy);
  const int * z = INTEGER(zz);
  const int ox = asInteger(oxx);
  const int oy = asInteger(oyy);
  const int oz = asInteger(ozz);
  const int x1 = asInteger(x11);
  const int y1 = asInteger(y11);
  const int z1 = asInteger(z11);
  const int x2 = asInteger(x22);
  const int y2 = asInteger(y22);
  const int z2 = asInteger(z22);

  const int * A = LOGICAL(AA);
  const int * B = LOGICAL(BB);
  const int * C = LOGICAL(CC);




  R_xlen_t nx = xlength(xx);
  R_xlen_t nA = xlength(AA);
  R_xlen_t n = (nx > nA) ? nx : nA;
  bool useX = xlength(xx) == n;
  bool useY = xlength(yy) == n;
  bool useZ = xlength(zz) == n;

  // Which variables are bare logicals
  bool A_lgl = xlength(AA) == n;
  bool B_lgl = xlength(BB) == n;
  bool C_lgl = xlength(CC) == n;

  // Is the 1st, 2nd, 3rd expression usable or should we just set it to false?
  bool e1 = useX || A_lgl;
  bool e2 = useY || B_lgl;
  bool e3 = useZ || C_lgl;

  // Are the expressions preceded by `!` -- i.e the opposite
  bool A_opposite = A_lgl && ox == 1;
  bool B_opposite = B_lgl && oy == 1;
  bool C_opposite = C_lgl && oz == 1;

  if (useX && A_lgl) {
    error("Internal error: useX && A_lgl"); // # nocov
  }
  if (useY && B_lgl) {
    error("Internal error: useY && B_lgl"); // # nocov
  }
  if (useZ && C_lgl) {
    error("Internal error: useZ && C_lgl"); // # nocov
  }

  if (!e1 && !e2 && !e3) {
    return LogicalN(n);  // # nocov
  }

  SEXP ans = PROTECT(allocVector(RAWSXP, n));
  unsigned char * out = RAW(ans);

  if (useX && useY && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (useX && useY && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && useY && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  // e3 = false
  if (useX && useY && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        isingle_ox_x1_x2(y[i], oy, y1, y2);
    }
    UNPROTECT(1); return ans;
  }

  // B_lgl (but B_opposite must always precede!)
  if (useX && B_opposite && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        !B[i] &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_lgl && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        B[i] &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_opposite && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        !B[i] &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_lgl && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        B[i] &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_opposite && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        !B[i] &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_lgl && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        B[i] &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_opposite && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        !B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_lgl && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && !e2 && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2);
    }
    UNPROTECT(1); return ans;
  }
  // // A_opposite

  if (A_opposite && useY && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && useY && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && useY && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  // e3 = false
  if (A_opposite && useY && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2);
    }
    UNPROTECT(1); return ans;
  }

  // B_lgl
  if (A_opposite && B_opposite && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        !B[i] &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_lgl && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        B[i] &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_opposite && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        !B[i] &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_lgl && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        B[i] &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_opposite && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        !B[i] &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_lgl && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        B[i] &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_opposite && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        !B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_lgl && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && !e2 && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i];
    }
    UNPROTECT(1); return ans;
  }

  // // A_lgl

  if (A_lgl && useY && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && useY && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && useY && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  // e3 = false
  if (A_lgl && useY && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2);
    }
    UNPROTECT(1); return ans;
  }

  // B_lgl
  if (A_lgl && B_opposite && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        !B[i] &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_lgl && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        B[i] &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_opposite && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        !B[i] &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_lgl && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        B[i] &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_opposite && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        !B[i] &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_lgl && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        B[i] &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_opposite && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        !B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_lgl && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && !e2 && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i];
    }
    UNPROTECT(1); return ans;
  }

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < n; ++i) {
    bool oi = false;

    // 1st expression
    if (e1) {
      oi = useX ? isingle_ox_x1_x2(x[i], ox, x1, x2) : (A_opposite ? !A[i] : A[i]);
      if (!oi) {
        out[i] = FALSE;
        continue;
      }
    }

    // 2nd expression
    // #nocov start
    if (e2) {
      oi = useY ? isingle_ox_x1_x2(y[i], oy, y1, y2) : (B_opposite ? !B[i] : B[i]);
      if (!oi) {
        out[i] = FALSE;
        continue;
      }
    }
    // #nocov end
    // 3rd expression
    if (e3) {
      oi = useZ ? isingle_ox_x1_x2(z[i], oz, z1, z2) : (C_opposite ? !C[i] : C[i]);
      if (!oi) {
        out[i] = FALSE;
        continue;
      }
    }

    out[i] = TRUE;

  }
  UNPROTECT(1);
  return ans;
}

SEXP Csum3s_par(SEXP xx, SEXP oxx, SEXP x11, SEXP x22,
                SEXP xxd, SEXP xd11, SEXP xd22,
                SEXP yy, SEXP oyy, SEXP y11, SEXP y22,
                SEXP yyd, SEXP yd11, SEXP yd22,
                SEXP zz, SEXP ozz, SEXP z11, SEXP z22,
                SEXP zzd, SEXP zd11, SEXP zd22,
                SEXP AA,
                SEXP BB,
                SEXP CC,
                SEXP Aampersand,
                SEXP nthreads) {


  // ampersand TRUE => sum_and3,  FALSE  => sum_or3
  const bool ampersand = asLogical(Aampersand);
  int nThread = as_nThread(nthreads);
  const int * x = INTEGER(xx);
  const int * y = INTEGER(yy);
  const int * z = INTEGER(zz);
  const double * xd = REAL(xxd);
  const double * yd = REAL(yyd);
  const double * zd = REAL(zzd);
  const int ox = asInteger(oxx);
  const int oy = asInteger(oyy);
  const int oz = asInteger(ozz);
  const int x1 = asInteger(x11);
  const int y1 = asInteger(y11);
  const int z1 = asInteger(z11);
  const double xd1 = asReal(xd11);
  const double yd1 = asReal(yd11);
  const double zd1 = asReal(zd11);
  const int x2 = asInteger(x22);
  const int y2 = asInteger(y22);
  const int z2 = asInteger(z22);
  const double xd2 = asReal(xd22);
  const double yd2 = asReal(yd22);
  const double zd2 = asReal(zd22);

  const int * A = LOGICAL(AA);
  const int * B = LOGICAL(BB);
  const int * C = LOGICAL(CC);

  R_xlen_t lengths[9] = {xlength(xx), xlength(xxd),
                         xlength(yy), xlength(yyd),
                         xlength(zz), xlength(zzd),
                         xlength(AA), xlength(BB), xlength(CC)};

  R_xlen_t n = xlength(xx);
  for (int i = 1; i < 9; ++i) {
    if (n < lengths[i]) {
      n = lengths[i];
    }
  }

  // 0 use none, 1 use int, 2 use double, 3 use logical, 4 use opposite
  const int Xcase = (xlength(xx) == n) + 2 * (xlength(xxd) == n) + 3 * (xlength(AA) == n && ox != OP_NE) + 4 * (xlength(AA) == n && ox == OP_NE);
  const int Ycase = (xlength(yy) == n) + 2 * (xlength(yyd) == n) + 3 * (xlength(BB) == n && oy != OP_NE) + 4 * (xlength(BB) == n && oy == OP_NE);
  const int Zcase = (xlength(zz) == n) + 2 * (xlength(zzd) == n) + 3 * (xlength(CC) == n && oz != OP_NE) + 4 * (xlength(CC) == n && oz == OP_NE);


  R_xlen_t out = 0;

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : out)
#endif
  for (R_xlen_t i = 0; i < n; ++i) {
    bool oi = Xcase == 0;

    // 1st expression
    switch(Xcase) {
    case 1:
      oi = isingle_ox_x1_x2(x[i], ox, x1, x2);
      break;
    case 2:
      oi = dsingle_ox_x1_x2(xd[i], ox, xd1, xd2);
      break;
    case 3:
      oi = A[i] == 1;
      break;
    case 4:
      oi = A[i] == 0;
      break;
    }
    if (ampersand) {
      if (!oi) {
        continue;
      }
    } else {
      if (oi) {
        out += 1;
        continue;
      }
    }


    // 2nd expression

    switch(Ycase) {
    case 1:
      oi = isingle_ox_x1_x2(y[i], oy, y1, y2);
      break;
    case 2:
      oi = dsingle_ox_x1_x2(yd[i], oy, yd1, yd2);
      break;
    case 3:
      oi = B[i] == 1;
      break;
    case 4:
      oi = B[i] == 0;
      break;
    }
    if (ampersand) {
      if (!oi) {
        continue;
      }
    } else {
      if (oi) {
        out += 1;
        continue;
      }
    }

    // 3rd expression
    switch(Zcase) {
    case 1:
      oi = isingle_ox_x1_x2(z[i], oz, z1, z2);
      break;
    case 2:
      oi = dsingle_ox_x1_x2(zd[i], oz, zd1, zd2);
      break;
    case 3:
      oi = C[i] == 1;
      break;
    case 4:
      oi = C[i] == 0;
      break;
    }
    if (ampersand) {
      if (!oi) {
        continue;
      }
      out += 1;
    } else {
      if (oi) {
        out += 1;
        continue;
      }
    }
  }
  return ScalarLength(out);
}

R_xlen_t wsum_and(SEXP X1, const int op1, SEXP Y1,
                  SEXP X2, const int op2, SEXP Y2,
                  int nThread) {

  switch(TYPEOF(X1)) {
  case INTSXP:
    switch(TYPEOF(Y1)) {
    case INTSXP:
      return xlength(X1);
    }
  }
  return 0;
}

SEXP which_int_ne_1i(const int * x, R_xlen_t N, int y, R_xlen_t ion) {
  int * o = malloc(sizeof(int) * ion);
  if (o == NULL) {
    free(o);
    return R_NilValue;
  }
  R_xlen_t oN = ion;
  unsigned int k = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    if (x[i] != y) {
      o[k++] = i + 1u;
      if ((k + (k >> 1) + (k >> 3) + 16) >= oN) {
        oN = (oN >> 1) + (oN >> 3);
        int * o_temp = realloc(o, sizeof(int) * oN);
        if (o_temp == NULL) {
          free(o);
          free(o_temp);
          return R_NilValue;
        }
        o = o_temp;
      }
    }
  }
  SEXP ans = PROTECT(allocVector(INTSXP, k));
  int * restrict ansp = INTEGER(ans);
  for (unsigned int j = 0; j < k; ++j) {
    ansp[j] = o[j];
  }
  free(o);
  UNPROTECT(1);
  return ans;
}

#define WHIC_CIN(O1, O2, Y1, Y2)                                 \
for (int i = 0; i < N; ++i) {                                    \
  if (x1p[i] O1 Y1 &&                                            \
      x2p[i] O2 Y2) {                                            \
    o[k++] = i + 1;                                              \
  }                                                              \
}                                                                \

#define WHIC_CIN_1_1(O1)                                \
switch(o2) {                                            \
case OP_NI:                                             \
case OP_NE:                                             \
  WHIC_CIN(O1, !=, y1_0, y2_0);                         \
  break;                                                \
case OP_IN:                                             \
case OP_EQ:                                             \
  WHIC_CIN(O1, ==, y1_0, y2_0);                         \
  break;                                                \
case OP_GE:                                             \
  WHIC_CIN(O1, >=, y1_0, y2_0);                         \
  break;                                                \
case OP_LE:                                             \
  WHIC_CIN(O1, <=, y1_0, y2_0);                         \
  break;                                                \
case OP_GT:                                             \
  WHIC_CIN(O1, >, y1_0, y2_0);                          \
  break;                                                \
case OP_LT:                                             \
  WHIC_CIN(O1, <, y1_0, y2_0);                          \
  break;                                                \
}                                                       \
break;                                                  \


#define WWHICN_CIN(O1)                                 \
switch(o2) {                                           \
case OP_NI:                                            \
case OP_NE:                                            \
  WHIC_CIN(O1, !=, y1p[i], y2_0);                      \
  break;                                               \
case OP_IN:                                            \
case OP_EQ:                                            \
  WHIC_CIN(O1, ==, y1p[i], y2_0);                      \
  break;                                               \
case OP_GE:                                            \
  WHIC_CIN(O1, >=, y1p[i], y2_0);                      \
  break;                                               \
case OP_LE:                                            \
  WHIC_CIN(O1, <=, y1p[i], y2_0);                      \
  break;                                               \
case OP_GT:                                            \
  WHIC_CIN(O1, >, y1p[i], y2_0);                       \
  break;                                               \
case OP_LT:                                            \
  WHIC_CIN(O1, <, y1p[i], y2_0);                       \
  break;                                               \
}                                                      \
break;                                                 \




#define WWHIC_CINN(O1)                                \
switch(o2) {                                          \
case OP_NE:                                           \
  WHIC_CIN(O1, !=, y1_0, y2p[i]);                     \
  break;                                              \
case OP_IN:                                           \
case OP_EQ:                                           \
  WHIC_CIN(O1, ==, y1_0, y2p[i]);                     \
  break;                                              \
case OP_GE:                                           \
  WHIC_CIN(O1, >=, y1_0, y2p[i]);                     \
  break;                                              \
case OP_LE:                                           \
  WHIC_CIN(O1, <=, y1_0, y2p[i]);                     \
  break;                                              \
case OP_GT:                                           \
  WHIC_CIN(O1, >, y1_0, y2p[i]);                      \
  break;                                              \
case OP_LT:                                           \
  WHIC_CIN(O1, <, y1_0, y2p[i]);                      \
  break;                                              \
}                                                     \
break;                                                \

#define WWHICN_CINN(O1)                                \
switch(o2) {                                           \
case OP_NE:                                            \
  WHIC_CIN(O1, !=, y1p[i], y2p[i]);                    \
  break;                                               \
case OP_IN:                                            \
case OP_EQ:                                            \
  WHIC_CIN(O1, ==, y1p[i], y2p[i]);                    \
  break;                                               \
case OP_GE:                                            \
  WHIC_CIN(O1, >=, y1p[i], y2p[i]);                    \
  break;                                               \
case OP_LE:                                            \
  WHIC_CIN(O1, <=, y1p[i], y2p[i]);                    \
  break;                                               \
case OP_GT:                                            \
  WHIC_CIN(O1, >, y1p[i], y2p[i]);                     \
  break;                                               \
case OP_LT:                                            \
  WHIC_CIN(O1, <, y1p[i], y2p[i]);                     \
  break;                                               \
}                                                      \
break;                                                 \



#define O2ANS                                                 \
SEXP ans = PROTECT(allocVector(INTSXP, k));                   \
int * restrict ansp = INTEGER(ans);                           \
for (int i = 0; i < k; ++i) {                                 \
  ansp[i] = o[i];                                             \
}                                                             \
free(o);                                                      \
UNPROTECT(1);                                                 \
return ans;                                                   \



static bool is_in(int x, const int * y, int M) {
  for (int i = 0; i < M; ++i) {
    if (y[i] == x) {
      return true;
    }
  }
  return false;
}

static bool isnt_in(int x, const int * y, int M) {
  for (int i = 0; i < M; ++i) {
    if (y[i] == x) {
      return false;
    }
  }
  return true;
}

static void which_INT_in(int * o, unsigned int * kp,
                         const int * x, int N,
                         const int * y, int M,
                         bool opposite) {
  unsigned int k = kp[0];
  unsigned int ko = k; // k out
  if (k) {
    // o has indices
    if (opposite) {
      for (int i = 0; i < k; ++i) {
        int j = o[i];
        int xpi = x[j];
        if (is_in(xpi, y, M)) {
          o[i] = 0;
          --ko;
        }
      }
    } else {
      for (int i = 0; i < k; ++i) {
        int j = o[i];
        int xpi = x[j];
        if (isnt_in(xpi, y, M)) {
          o[i] = 0;
          --ko;
        }
      }
    }
  } else {
    if (opposite) {
      for (int i = 0; i < N; ++i) {
        if (isnt_in(x[i], y, M)) {
          o[ko++] = i + 1;
        }
      }
    } else {
      for (int i = 0; i < N; ++i) {
        if (is_in(x[i], y, M)) {
          o[ko++] = i + 1;
        }
      }
    }
  }
  kp[0] = ko;

}

static void collapse_o(int * o, unsigned int N) {
  unsigned int j = 0;
  for (unsigned int i = 0; i < N; ++i) {
    o[j] = o[i];
    j += o[i] > 0;
  }
}

SEXP which_iiii(SEXP X1, const int o1, SEXP Y1,
                SEXP X2, const int o2, SEXP Y2,
                int ion) {
  int N = length(X1);
  if (N == 0) {
    return IntegerN(0);
  }
  const int * x1p = INTEGER(X1);
  const int * x2p = INTEGER(X2);
  const int * y1p = INTEGER(Y1);
  const int * y2p = INTEGER(Y2);

  int * o = malloc(ion * sizeof(int));
  if (o == NULL) {
    free(o);
    return R_NilValue;
  }
  memset(o, 0, sizeof(int) * ion);
  if (((o1 == OP_IN) || (o1 == OP_NI)) &&
      ((o2 == OP_IN) || (o2 == OP_NI))) {
    unsigned int kk[1] = {0};
    which_INT_in(o, kk, x1p, N, y1p, length(Y1), o1 != OP_IN);
    which_INT_in(o, kk, x2p, N, y2p, length(Y2), o2 != OP_IN);
    unsigned int k = kk[0];
    collapse_o(o, k);
    O2ANS;
  }

  int cf1 = cf_xlen(X1, Y1);
  int cf2 = cf_xlen(X2, Y2);
  if (cf1 == 0 || cf2 == 0) {
    // Maybe we're using %in% or %notin%

    free(o);
    return R_NilValue; // bad recycling

  }


  const int y1_0 = y1p[0];
  const int y2_0 = y2p[0];
  // for use in length-2 RHS (e.g. %between%)
  const int y1_1 = (cf1 != 1) ? y1p[1] : y1_0;
  const int y2_1 = (cf2 != 1) ? y2p[1] : y2_0;

  unsigned int k = 0;
  switch(cf1) {
  case CF_LEN_1:
    switch(cf2) {
    case CF_LEN_1:
      switch(o1) {
      case OP_NI:
      case OP_NE:
        WHIC_CIN_1_1(!=)
      case OP_IN:
      case OP_EQ:
        WHIC_CIN_1_1(==)
      case OP_GE:
        WHIC_CIN_1_1(>=)
      case OP_LE:
        WHIC_CIN_1_1(<=)
      case OP_GT:
        WHIC_CIN_1_1(>)
      case OP_LT:
        WHIC_CIN_1_1(<)
      }
      break;
    case CF_LEN_2:
      switch(o1) {
      case OP_NI:
      case OP_NE:
        switch(o2) {
        case OP_NI:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] != y1_0 &&
                x2p[i] != y2_0 && x2p[i] != y2_1) {
              o[k++] = i + 1;
            }
          }
          break;
        case OP_IN:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] != y1_0 &&
                x2p[i] == y2_0 && x2p[i] == y2_1) {
              o[k++] = i + 1;
            }
          }
          break;
        case OP_BW:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] != y1_0 &&
                (x2p[i] >= y2_0 || x2p[i] <= y2_1)) {
              o[k++] = i + 1;
            }
          }
          break;
        case OP_BO:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] != y1_0 &&
                (x2p[i] > y2_0 || x2p[i] < y2_1)) {
              o[k++] = i + 1;
            }
          }
          break;
        case OP_BC:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] != y1_0 &&
                (x2p[i] <= y2_0 || x2p[i] >= y2_1)) {
              o[k++] = i + 1;
            }
          }
          break;
        }
        break;
      }
      break;
    case CF_LEN_N:
      switch(o1) {
      case OP_NI:
      case OP_NE:
        WWHIC_CINN(!=)
      case OP_IN:
      case OP_EQ:
        WWHIC_CINN(==)
      case OP_GE:
        WWHIC_CINN(>=)
      case OP_LE:
        WWHIC_CINN(<=)
      case OP_GT:
        WWHIC_CINN(>)
      case OP_LT:
        WWHIC_CINN(<)
      }
      break;
    }
    break;
  case CF_LEN_2:
    switch(cf2) {
    case CF_LEN_1:
      switch(o1) {
      case OP_IN:
        switch(o2) {
        case OP_NI:
        case OP_NE:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] == y1_0 || x1p[i] == y1_1) {
              if (x2p[i] != y2_0) {
                o[k++] = i + 1;
              }
            }
          }
          break;
        case OP_IN:
        case OP_EQ:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] == y1_0 || x1p[i] == y1_1) {
              if (x2p[i] == y2_0) {
                o[k++] = i + 1;
              }
            }
          }
          break;
        case OP_GE:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] == y1_0 || x1p[i] == y1_1) {
              if (x2p[i] >= y2_0) {
                o[k++] = i + 1;
              }
            }
          }
          break;
        case OP_LE:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] == y1_0 || x1p[i] == y1_1) {
              if (x2p[i] <= y2_0) {
                o[k++] = i + 1;
              }
            }
          }
          break;
        case OP_GT:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] == y1_0 || x1p[i] == y1_1) {
              if (x2p[i] > y2_0) {
                o[k++] = i + 1;
              }
            }
          }
          break;
        case OP_LT:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] == y1_0 || x1p[i] == y1_1) {
              if (x2p[i] < y2_0) {
                o[k++] = i + 1;
              }
            }
          }
          break;

        }
        break;
      case OP_NI:
        switch(o2) {
        case OP_NI:
        case OP_NE:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] != y1_0 && x1p[i] != y1_1) {
              if (x2p[i] != y2_0) {
                o[k++] = i + 1;
              }
            }
          }
          break;
        case OP_IN:
        case OP_EQ:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] != y1_0 && x1p[i] != y1_1) {
              if (x2p[i] == y2_0) {
                o[k++] = i + 1;
              }
            }
          }
          break;
        case OP_GE:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] != y1_0 && x1p[i] != y1_1) {
              if (x2p[i] >= y2_0) {
                o[k++] = i + 1;
              }
            }
          }
          break;
        case OP_LE:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] != y1_0 && x1p[i] != y1_1) {
              if (x2p[i] <= y2_0) {
                o[k++] = i + 1;
              }
            }
          }
          break;
        case OP_GT:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] != y1_0 && x1p[i] != y1_1) {
              if (x2p[i] > y2_0) {
                o[k++] = i + 1;
              }
            }
          }
          break;
        case OP_LT:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] == y1_0 && x1p[i] == y1_1) {
              if (x2p[i] < y2_0) {
                o[k++] = i + 1;
              }
            }
          }
          break;

        }
        break;
      case OP_BW:
        switch(o2) {
        case OP_NI:
        case OP_NE:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] != y2_0) {
              o[k++] = i + 1;
            }
          }
          break;
        case OP_IN:
        case OP_EQ:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] == y2_0) {
              o[k++] = i + 1;
            }
          }
          break;
        case OP_GE:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] >= y2_0) {
              o[k++] = i + 1;
            }
          }
          break;
        case OP_LE:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] <= y2_0) {
              o[k++] = i + 1;
            }
          }
          break;
        case OP_GT:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] > y2_0) {
              o[k++] = i + 1;
            }
          }
          break;
        case OP_LT:
          for (int i = 0; i < N; ++i) {
            if (x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] < y2_0) {
              o[k++] = i + 1;
            }
          }
          break;
        }
        break;
      default:
        for (int i = 0; i < N; ++i) {
          if (x1p[i] >= y1_0 && x1p[i] <= y1_1 && isingle_ox_x1_x2(x2p[i], o2, y2_0, y2_1)) {
            o[k++] = i + 1;
          }
        }
      }
    case CF_LEN_2:
      for (int i = 0; i < N; ++i) {
        if (isingle_ox_x1_x2(x1p[i], o1, y1_0, y1_1) &&
            isingle_ox_x1_x2(x2p[i], o2, y2_0, y2_1)) {
          o[k++] = i + 1;
        }
      }
      break;
    case CF_LEN_N:
      for (int i = 0; i < N; ++i) {
        if (isingle_ox_x1_x2(x1p[i], o1, y1_0, y1_1) &&
            isingle_ox_x1_x2(x2p[i], o2, y2p[i], y2p[i])) {
          o[k++] = i + 1;
        }
      }
      break;
    }
    break;
  case CF_LEN_N:
    switch(cf2) {
    case CF_LEN_1:
    case CF_LEN_2:
      for (int i = 0; i < N; ++i) {
        if (isingle_ox_x1_x2(x1p[i], o1, y1p[i], y1p[i]) &&
            isingle_ox_x1_x2(x2p[i], o2, y2_0, y2_1)) {
          o[k++] = i + 1;
        }
      }
      break;
    case CF_LEN_N:
      for (int i = 0; i < N; ++i) {
        if (isingle_ox_x1_x2(x1p[i], o1, y1p[i], y1p[i]) &&
            isingle_ox_x1_x2(x2p[i], o2, y2p[i], y2p[i])) {
          o[k++] = i + 1;
        }
      }
      break;
    }
    break;
  }

  if (k) {
    SEXP ans = PROTECT(allocVector(INTSXP, k));
    int * restrict ansp = INTEGER(ans);
    for (int j = 0; j < k; ++j) {
      ansp[j] = o[j];
    }
    free(o);
    UNPROTECT(1);
    return ans;
  }
  free(o);
  return IntegerN(0);
}

SEXP C_which_and1s(SEXP O1, SEXP X1, SEXP X2, SEXP Ion) {

  if (!isString(O1) || xlength(O1) == 1) {
    return R_NilValue;
  }
  const char * oc = CHAR(STRING_ELT(O1, 0));
  int o = do_op2M(oc);
  if (o == 0) {
    return R_NilValue;
  }
  int lenc = cf_xlen(X1, X2);
  R_xlen_t ion = (Ion == R_NilValue) ? 1024 : asReal(Ion);

  switch(TYPEOF(X1)) {
  case INTSXP:
    switch(lenc) {
    case CF_LEN_1:
      switch(TYPEOF(X2)) {
      case INTSXP:
        return which_int_ne_1i(INTEGER(X1), xlength(X1), asInteger(X2), ion);
      }
    }
  }
  return R_NilValue;
}


SEXP C_which_and2s(SEXP X1, SEXP O1, SEXP Y1,
                   SEXP X2, SEXP O2, SEXP Y2,
                   SEXP nthread,
                   SEXP Ion) {
  if (xlength(X1) != xlength(X2)) {
    error("lengths differ, X1, X2");
  }
  if (xlength(X1) >= INT_MAX) {
    return R_NilValue;
  }
  int ion = asInteger(Ion);
  int N = length(X1);
  if (N == 0) {
    return IntegerN(0);
  }
  const int op1 = asInteger(O1);
  const int op2 = asInteger(O2);


  switch(TYPEOF(X1)) {
  case INTSXP:
    switch(TYPEOF(X2)) {
    case INTSXP:
      switch(TYPEOF(Y1)) {
      case INTSXP:
        switch(TYPEOF(Y2)) {
        case INTSXP:
          return which_iiii(X1, op1, Y1,
                            X2, op2, Y2,
                            ion);
        }
      }
    }
  }
  return R_NilValue;

}


SEXP C_which_raw(SEXP X, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(X);
  const unsigned char * xp = RAW(X);
  R_xlen_t o = 0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : o)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    o += xp[i];
  }
  if (o < INT_MAX) {
    SEXP ans = PROTECT(allocVector(INTSXP, o));
    int * restrict ansp = INTEGER(ans);
    int j = 0;
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[j] = i + 1;
      j += (int)xp[i];
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
    j += (int)xp[i];
  }
  UNPROTECT(1);
  return ans;

}

