#include "hutilscpp.h"


static void sum_rr_et_rr(R_xlen_t * ans,
                         int * err,
                         const int o1,
                         const double * x1, R_xlen_t N1,
                         const double * y1, R_xlen_t M1,
                         const int o2,
                         const double * x2, R_xlen_t N2,
                         const double * y2, R_xlen_t M2,
                         int nThread) {
  if (N1 == 1 && N2 == 1) {
    ans[0] = dsingle_ox_x1_x2(x1[0], o1, y1[0], y1[0]) && dsingle_ox_x1_x2(x2[0], o2, y2[0], y2[0]);
    return;
  }

  R_xlen_t N = (N1 >= N2) ? N1 : N2;

  if ((N1 != 1 && N1 != N) || (N2 != 1 && N2 != N)) {
    err[0] = INCOMPAT_LEN;
    return;
  }
  R_xlen_t o = 0;
  if (M1 == 1) {
    if (M2 == 2) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[0], y1[0]) && dsingle_ox_x1_x2(x2[i], o2, y2[0], y2[1]);)
    } else if (M2 == 1) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[0], y1[0]) && dsingle_ox_x1_x2(x2[i], o2, y2[0], y2[0]);)
    } else if (M2 == N) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[0], y1[0]) && dsingle_ox_x1_x2(x2[i], o2, y2[i], y2[i]);)
    }
  } else if (M1 == 2) {
    if (M2 == 2) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[0], y1[1]) && dsingle_ox_x1_x2(x2[i], o2, y2[0], y2[1]);)
    } else if (M2 == 1) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[0], y1[1]) && dsingle_ox_x1_x2(x2[i], o2, y2[0], y2[0]);)
    } else if (M2 == N) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[0], y1[1]) && dsingle_ox_x1_x2(x2[i], o2, y2[i], y2[i]);)
    }
  } else if (M1 == N) {
    if (M2 == 2) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[i], y1[i]) && dsingle_ox_x1_x2(x2[i], o2, y2[0], y2[1]);)
    } else if (M2 == 1) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[i], y1[i]) && dsingle_ox_x1_x2(x2[i], o2, y2[0], y2[0]);)
    } else if (M2 == N) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[i], y1[i]) && dsingle_ox_x1_x2(x2[i], o2, y2[i], y2[i]);)
    }
  }
  ans[0] = o;
}

static void sum_rr_or_rr(R_xlen_t * ans,
                         int * err,
                         const int o1,
                         const double * x1, R_xlen_t N1,
                         const double * y1, R_xlen_t M1,
                         const int o2,
                         const double * x2, R_xlen_t N2,
                         const double * y2, R_xlen_t M2,
                         int nThread) {
  if (N1 == 1 && N2 == 1) {
    ans[0] = dsingle_ox_x1_x2(x1[0], o1, y1[0], y1[0]) || dsingle_ox_x1_x2(x2[0], o2, y2[0], y2[0]);
    return;
  }

  R_xlen_t N = (N1 >= N2) ? N1 : N2;

  if ((N1 != 1 && N1 != N) || (N2 != 1 && N2 != N)) {
    err[0] = INCOMPAT_LEN;
    return;
  }
  R_xlen_t o = 0;
  if (o1 == OP_GT && o2 == OP_GT) {
    if (M1 == 1 && M2 == 1) {
      const double y1_0 = y1[0];
      const double y2_0 = y2[0];
      FORLOOP_redsum(o += x1[i] > y1_0 || x2[i] > y2_0;)
      ans[0] = o;
      return;
    }
  }
  if (M1 == 1) {
    if (M2 == 2) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[0], y1[0]) || dsingle_ox_x1_x2(x2[i], o2, y2[0], y2[1]);)
    } else if (M2 == 1) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[0], y1[0]) || dsingle_ox_x1_x2(x2[i], o2, y2[0], y2[0]);)
    } else if (M2 == N) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[0], y1[0]) || dsingle_ox_x1_x2(x2[i], o2, y2[i], y2[i]);)
    }
  } else if (M1 == 2) {
    if (M2 == 2) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[0], y1[1]) || dsingle_ox_x1_x2(x2[i], o2, y2[0], y2[1]);)
    } else if (M2 == 1) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[0], y1[1]) || dsingle_ox_x1_x2(x2[i], o2, y2[0], y2[0]);)
    } else if (M2 == N) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[0], y1[1]) || dsingle_ox_x1_x2(x2[i], o2, y2[i], y2[i]);)
    }
  } else if (M1 == N) {
    if (M2 == 2) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[i], y1[i]) || dsingle_ox_x1_x2(x2[i], o2, y2[0], y2[1]);)
    } else if (M2 == 1) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[i], y1[i]) || dsingle_ox_x1_x2(x2[i], o2, y2[0], y2[0]);)
    } else if (M2 == N) {
      FORLOOP_redsum(o += dsingle_ox_x1_x2(x1[i], o1, y1[i], y1[i]) || dsingle_ox_x1_x2(x2[i], o2, y2[i], y2[i]);)
    }
  }
  ans[0] = o;
}

SEXP Csum_or2(SEXP oo1, SEXP xx1, SEXP yy1,
              SEXP oo2, SEXP xx2, SEXP yy2,
              SEXP nthreads) {
  const int o1 = sex2op(oo1);
  const int o2 = sex2op(oo2);
  int nThread = as_nThread(nthreads);
  R_xlen_t o[1] = {0};
  int err[1] = {0};
  if (!isReal(xx1) || !isReal(yy1) || !isReal(xx2) || !isReal(yy2)) {
    return R_NilValue;
  }
  switch(TYPEOF(xx1)) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
    switch(TYPEOF(yy1)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
      switch(TYPEOF(xx1)) {
      case LGLSXP:
      case INTSXP:
      case REALSXP:
        switch(TYPEOF(yy2)) {
        case LGLSXP:
        case INTSXP:
        case REALSXP:
          sum_rr_or_rr(o, err,
                       o1, REAL(xx1), xlength(xx1), REAL(yy1), xlength(yy1),
                       o2, REAL(xx2), xlength(xx2), REAL(yy2), xlength(yy2),
                       nThread);
        }
      }
    }
  }
  if (err[0]) {
    error("Error %d.", err[0]);
  }
  return ScalarLength(o[0]);
}
