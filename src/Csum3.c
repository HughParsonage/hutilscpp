#include "hutilscpp.h"



static bool op2b(const int o, int x, int y) {
  switch(o) {
  case OP_TRUE:
    return true;
  case OP_NE:
    return x != y;
  case OP_EQ:
    return x == y;
  case OP_GE:
    return x >= y;
  case OP_LE:
    return x <= y;
  case OP_GT:
    return x > y;
  case OP_LT:
    return x < y;
  }
  return 0;
}

static bool op2bb(const int o, int x, int y0, int y1) {
  switch(o) {
  case OP_BW:
    return betweeniiuu(x, y0, y1);
  case OP_BO:
    return betweeniiuu(x, y0 + 1u, y0 - 1u);
  case OP_BC:
    return !betweeniiuu(x, y0 + 1u, y0 - 1u);
  }
  return 0;
}

static bool And3_111(const int o1, int x1, int y1,
                     const int o2, int x2, int y2,
                     const int o3, int x3, int y3) {
  return
  op2b(o1, x1, y1) &&
    op2b(o2, x2, y2) &&
    op2b(o3, x3, y3);

}

static bool iAnd3_VVV(R_xlen_t i,
                      const int o1, const int * x1p, const int * y1p,
                      const int o2, const int * x2p, const int * y2p,
                      const int o3, const int * x3p, const int * y3p) {
  bool o = false;
  int x1pi = x1p[i], y1pi = y1p[i];
  int x2pi = x2p[i], y2pi = y2p[i];
  o = And3_111(o1, x1pi, y1pi,
               o2, x2pi, y2pi,
               o3, x3p[i], y3p[i]);
  return o;
}



R_xlen_t sum_lgl(const int * x, R_xlen_t N, const int op, int nThread) {
  R_xlen_t o = 0;
  if (op == OP_EQ) {
    FORLOOP_redsum(o += x[i] != 0;)
  } else {
    FORLOOP_redsum(o += x[i] == 0;)
  }
  return o;
}

R_xlen_t sum_ii(const int op,
                const int * x, R_xlen_t N,
                const int * y, R_xlen_t M,
                int nThread) {
  R_xlen_t o = 0;
  if (M == 1) {
    const int y0 = y[0];
    switch(op) {
    case OP_IN:
    case OP_EQ:
      FORLOOP_redsum(o += x[i] == y0;)
      break;
    case OP_NI:
    case OP_NE:
      FORLOOP_redsum(o += x[i] != y0;)
      break;
    case OP_GE:
      FORLOOP_redsum(o += x[i] >= y0;)
      break;
    case OP_LE:
      FORLOOP_redsum(o += x[i] <= y0;)
      break;
    case OP_GT:
      FORLOOP_redsum(o += x[i] > y0;)
      break;
    case OP_LT:
      FORLOOP_redsum(o += x[i] < y0;)
      break;
    }
  } else if (M == 2 && (op == OP_BW || op == OP_BO || op == OP_BC)) {
    int a = y[0];  // if NA, then no problem
    int b = y[1] == NA_INTEGER ? INT_MAX : y[1];
    if (a > b) {
      switch(o) {
      case OP_BW:
        return 0;
      case OP_BO:
        return 0;
      case OP_BC:
        return N;
      }
    }
    if (a == INT_MAX) {
      switch(o) {
      case OP_BW:
        FORLOOP_redsum(o += x[i] == INT_MAX;)
        break;
      case OP_BO:
        return 0;
      case OP_BC:
        return N;
      }
    }
    switch(o) {
    case OP_BW:
      FORLOOP_redsum(o += betweeniiuu(x[i], a, b);)
      break;
    case OP_BO:
      FORLOOP_redsum(o += betweeniiuu(x[i], a + 1, b - 1);) // integer overflow handled above
      break;
    case OP_BC:
      FORLOOP_redsum(o += x[i] <= a || x[i] >= b;)
      break;
    }
  } else if (op == OP_IN || op == OP_NI) {
    const bool op_in = op == OP_IN;
    if (M <= MAX_NAIVE_IN) {
      int MM = M;
      FORLOOP_redsum(
        int xi = x[i];
      bool is_in = false;
      for (int j = 0; j < MM; ++j) {
        if (xi == y[j]) {
          is_in = true;
          break;
        }
      }
      o += is_in != op_in;
      )
    }
  } else if (M == N) {
    switch(op) {
    case OP_EQ:
      FORLOOP_redsum(o += x[i] == y[i];)
      break;
    case OP_NE:
      FORLOOP_redsum(o += x[i] != y[i];)
      break;
    case OP_GE:
      FORLOOP_redsum(o += x[i] >= y[i];)
      break;
    case OP_LE:
      FORLOOP_redsum(o += x[i] <= y[i];)
      break;
    case OP_GT:
      FORLOOP_redsum(o += x[i] > y[i];)
      break;
    case OP_LT:
      FORLOOP_redsum(o += x[i] < y[i];)
      break;
    }
  } else {
    // do nothing bad recycling
  }
  return o;
}
R_xlen_t sum_id(const int op,
                const int * x, R_xlen_t N,
                const double * y, R_xlen_t M,
                int nThread) {
  R_xlen_t o = 0;
  if (M == 1) {

    if (do_is_safe2int(y[0])) {
      int yy[1] = {(int)y[0]};
      return sum_ii(op, x, N, (const int *)yy, 1, nThread);
    }

    const double y0 = y[0]; // either not a whole number or outside range

    // outside range is easiest to check
    if (y0 + 1 > INT_MAX) {
      if (op == OP_LT || op == OP_LE) {
        return N;
      }
      if (op == OP_GT || op == OP_LE) {
        return 0;
      }
    }
    if (y0 - 1 < INT_MIN) {
      if (op == OP_LT || op == OP_LE) {
        return 0;
      }
      if (op == OP_GT || op == OP_LE) {
        return N;
      }
    }
    const int y_ceil = (y0 > 0) ? y0 + 1 : y0 - 1;
    const int y_floo = (y0 > -1 && y0 < 1) ? 0 : (y0 > 0 ? y0 : y0 + 1);

    switch(op) {
    case OP_IN:
    case OP_EQ:
      return 0; // cannot ever be equal
      break;
    case OP_NI:
    case OP_NE:
      return N; // must always be unequal
      break;
    case OP_GE:
    case OP_GT:
      FORLOOP_redsum(o += x[i] >= y_ceil;)
      break;
    case OP_LE:
    case OP_LT:
      FORLOOP_redsum(o += x[i] <= y_floo;)
      break;
    }
  } else if (M == 2 && (op == OP_BW || op == OP_BO || op == OP_BC)) {
    double a = ISNAN(y[0]) ? R_NegInf : y[0];  // if NA, then no problem
    double b = ISNAN(y[1]) ? R_PosInf : y[1];
    if (a > b) {
      switch(o) {
      case OP_BW:
        return 0;
      case OP_BO:
        return 0;
      case OP_BC:
        return N;
      }
    }
    switch(o) {
    case OP_BW:
      FORLOOP_redsum(o += x[i] >= a && x[i] <= b;)
      break;
    case OP_BO:
      FORLOOP_redsum(o += x[i] > a && x[i] < b;)
      break;
    case OP_BC:
      FORLOOP_redsum(o += x[i] < a || x[i] > b;)
      break;
    }
  } else if (op == OP_IN || op == OP_NI) {
    const bool op_in = op == OP_IN;
    if (M <= MAX_NAIVE_IN) {
      int MM = M;
      FORLOOP_redsum(
        int xi = x[i];
      bool is_in = false;
      for (int j = 0; j < MM; ++j) {
        if (xi == y[j]) {
          is_in = true;
          break;
        }
      }
      o += is_in != op_in;
      )
    }
  } else if (M == N) {
    switch(op) {
    case OP_EQ:
      FORLOOP_redsum(o += x[i] == y[i];)
      break;
    case OP_NE:
      FORLOOP_redsum(o += x[i] != y[i];)
      break;
    case OP_GE:
      FORLOOP_redsum(o += x[i] >= y[i];)
      break;
    case OP_LE:
      FORLOOP_redsum(o += x[i] <= y[i];)
      break;
    case OP_GT:
      FORLOOP_redsum(o += x[i] > y[i];)
      break;
    case OP_LT:
      FORLOOP_redsum(o += x[i] < y[i];)
      break;
    }
  } else {
    // do nothing bad recycling
  }
  return o;
}

R_xlen_t sum_di(const int op,
                const double * x, R_xlen_t N,
                const int * y, R_xlen_t M,
                int nThread) {
  R_xlen_t o = 0;
  if (M == 1) {
    const double y0 = y[0];
    switch(op) {
    case OP_IN:
    case OP_EQ:
      FORLOOP_redsum(o += x[i] == y0;)
      break;
    case OP_NI:
    case OP_NE:
      FORLOOP_redsum(o += x[i] != y0;)
      break;
    case OP_GE:
      FORLOOP_redsum(o += x[i] >= y0;)
      break;
    case OP_LE:
      FORLOOP_redsum(o += x[i] <= y0;)
      break;
    case OP_GT:
      FORLOOP_redsum(o += x[i] > y0;)
      break;
    case OP_LT:
      FORLOOP_redsum(o += x[i] < y0;)
      break;
    }
  }
  return o;
}

R_xlen_t sum_dd(const int op,
                const double * x, R_xlen_t N,
                const double * y, R_xlen_t M,
                int nThread) {
  R_xlen_t o = 0;
  if (M == 1) {
    const double y0 = y[0];
    switch(op) {
    case OP_IN:
    case OP_EQ:
      FORLOOP_redsum(o += x[i] == y0;)
      break;
    case OP_NI:
    case OP_NE:
      FORLOOP_redsum(o += x[i] != y0;)
      break;
    case OP_GE:
      FORLOOP_redsum(o += x[i] >= y0;)
      break;
    case OP_LE:
      FORLOOP_redsum(o += x[i] <= y0;)
      break;
    case OP_GT:
      FORLOOP_redsum(o += x[i] > y0;)
      break;
    case OP_LT:
      FORLOOP_redsum(o += x[i] < y0;)
      break;
    }
  } else if (M == 2 && (op == OP_BW || op == OP_BO || op == OP_BC)) {
    double a = ISNAN(y[0]) ? R_NegInf : y[0];  // if NA, then no problem
    double b = ISNAN(y[1]) ? R_PosInf : y[1];
    if (a > b) {
      switch(o) {
      case OP_BW:
        return 0;
      case OP_BO:
        return 0;
      case OP_BC:
        return N;
      }
    }
    switch(o) {
    case OP_BW:
      FORLOOP_redsum(o += x[i] >= a && x[i] <= b;)
      break;
    case OP_BO:
      FORLOOP_redsum(o += x[i] > a && x[i] < b;)
      break;
    case OP_BC:
      FORLOOP_redsum(o += x[i] < a || x[i] > b;)
      break;
    }
  } else if (op == OP_IN || op == OP_NI) {
    const bool op_in = op == OP_IN;
    if (M <= MAX_NAIVE_IN) {
      int MM = M;
      FORLOOP_redsum(
        double xi = x[i];
      bool is_in = false;
      for (int j = 0; j < MM; ++j) {
        if (xi == y[j]) {
          is_in = true;
          break;
        }
      }
      o += is_in != op_in;
      )
    }
  } else if (M == N) {
    switch(op) {
    case OP_EQ:
      FORLOOP_redsum(o += x[i] == y[i];)
      break;
    case OP_NE:
      FORLOOP_redsum(o += x[i] != y[i];)
      break;
    case OP_GE:
      FORLOOP_redsum(o += x[i] >= y[i];)
      break;
    case OP_LE:
      FORLOOP_redsum(o += x[i] <= y[i];)
      break;
    case OP_GT:
      FORLOOP_redsum(o += x[i] > y[i];)
      break;
    case OP_LT:
      FORLOOP_redsum(o += x[i] < y[i];)
      break;
    }
  } else {
    // do nothing bad recycling
  }
  return o;
}

R_xlen_t sum1(const int op, SEXP x1, SEXP y1, int nThread) {
  if (xlength(x1) == xlength(y1) && inv_op(op)) {
    // if it's real then integer then we just use real then integer
    // (provided op is inverted)
    if (TYPEOF(x1) == REALSXP && TYPEOF(y1) == INTSXP) {
      return sum1(inv_op(op), y1, x1, nThread);
    }
  }
  switch(TYPEOF(x1)) {
  case LGLSXP:
    return sum_lgl(LOGICAL(x1), xlength(x1), op, nThread);
  case INTSXP:
    switch(TYPEOF(y1)) {
    case INTSXP:
      return sum_ii(op, INTEGER(x1), xlength(x1), INTEGER(y1), xlength(y1), nThread);
    case REALSXP:
      return sum_id(op, INTEGER(x1), xlength(x1), REAL(y1), xlength(y1), nThread);

    }
  case REALSXP:
    switch(TYPEOF(y1)) {
    case INTSXP:
      return sum_di(op, REAL(x1), xlength(x1), INTEGER(y1), xlength(y1), nThread); // note only N != M
    case REALSXP:
      return sum_dd(op, REAL(x1), xlength(x1), REAL(y1), xlength(y1), nThread);
    }
  }
  return SUM1_UNSUPPORTED_TYPE;
}

R_xlen_t sum3(SEXP oo1, SEXP x1, SEXP y1,
              SEXP oo2, SEXP x2, SEXP y2,
              SEXP oo3, SEXP x3, SEXP y3,
              SEXP AAnd,
              SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  const bool And = asLogical(AAnd);
  const int o1 = sex2op(oo1);
  const int o2 = sex2op(oo3);
  const int o3 = sex2op(oo3);
  R_xlen_t o = 0;
  return o;
}

R_xlen_t sum_raw(const unsigned char * x, R_xlen_t N, int nThread) {
  R_xlen_t o = 0;
  FORLOOP_redsum(o += x[i];)
    return o;
}

R_xlen_t sum3_raw(const unsigned char * x,
                  const unsigned char * y,
                  const unsigned char * z,
                  R_xlen_t N, int nThread) {
  R_xlen_t o = 0;
  FORLOOP_redsum(o += x[i] & y[i] & z[i];)
    return o;
}

R_xlen_t max3_xlen(R_xlen_t N1, R_xlen_t N2, R_xlen_t N3) {
  if (N1 >= N2 && N1 >= N3) {
    return N1;
  }
  if (N2 >= N1 && N2 >= N3) {
    return N2;
  }
  return N3;
}

R_xlen_t sum_lll(const int o1, const int * x1p, R_xlen_t N1,
                 const int o2, const int * x2p, R_xlen_t N2,
                 const int o3, const int * x3p, R_xlen_t N3,
                 int nThread) {
  R_xlen_t o = 0;
  R_xlen_t N = max3_xlen(N1, N2, N3);

  if (N1 == N && N2 == N && N3 == N) {
    if (o1 == OP_EQ) {
      if (o2 == OP_EQ) {
        if (o3 == OP_EQ) {
          FORLOOP_redsum(o += x1p[i] & x2p[i] & x3p[i];)
        } else {
          FORLOOP_redsum(o += x1p[i] & x2p[i] & (x3p[i] == 0);)
        }
      } else {
        if (o3 == OP_EQ) {
          FORLOOP_redsum(o += x1p[i] & (x2p[i] == 0) & x3p[i];)
        } else {
          FORLOOP_redsum(o += x1p[i] & (x2p[i] == 0) & (x3p[i] == 0);)
        }
      }
    } else {
      if (o2 == OP_EQ) {
        if (o3 == OP_EQ) {
          FORLOOP_redsum(o += (x1p[i] == 0) & x2p[i] & x3p[i];)
        } else {
          FORLOOP_redsum(o += (x1p[i] == 0) & x2p[i] & (x3p[i] == 0);)
        }
      } else {
        if (o3 == OP_EQ) {
          FORLOOP_redsum(o += (x1p[i] == 0) & x2p[i] & x3p[i];)
        } else {
          FORLOOP_redsum(o += (x1p[i] == 0) & (x2p[i] == 0) & x3p[i];)
        }
      }
    }
  } else if (N2 == N && N3 == 1) {

    if (x3p[0] == NA_INTEGER) {
      return 0;
    }
    const int x3p0 = x3p[0] == 0 ? 0 : 1;
    if (o1 == OP_EQ) {
      if (o2 == OP_EQ) {
        if (o3 == OP_EQ) {
          FORLOOP_redsum(o += x1p[i] & x2p[i] & x3p0;)
        } else {
          FORLOOP_redsum(o += x1p[i] & x2p[i] & !x3p0;)
        }
      } else {
        if (o3 == OP_EQ) {
          FORLOOP_redsum(o += x1p[i] & (x2p[i] == 0) & x3p0;)
        } else {
          FORLOOP_redsum(o += x1p[i] & (x2p[i] == 0) & !x3p0;)
        }
      }
    } else {
      if (o2 == OP_EQ) {
        if (o3 == OP_EQ) {
          FORLOOP_redsum(o += (x1p[i] == 0) & x2p[i] & x3p0;)
        } else {
          FORLOOP_redsum(o += (x1p[i] == 0) & x2p[i] & !x3p0;)
        }
      } else {
        if (o3 == OP_EQ) {
          FORLOOP_redsum(o += (x1p[i] == 0) & x2p[i] & x3p0;)
        } else {
          FORLOOP_redsum(o += (x1p[i] == 0) & (x2p[i] == 0) & !x3p0;)
        }
      }
    }
  } else {
    return -1;
  }
  return o;
}

R_xlen_t sum_LLI(const int o1, const int * x1p, R_xlen_t N1,
                 const int o2, const int * x2p, R_xlen_t N2,
                 const int o3, const int * x3p, const int * y3p,
                 int nThread) {
  R_xlen_t N = (N1 >= N2) ? N1 : N2;
  if (N != N1 || N != N2) {
    return -1;
  }
  R_xlen_t o = 0;
  if (o1 == OP_EQ) {
    if (o2 == OP_EQ) {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] == y3p[i]);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] != y3p[i]);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] >= y3p[i]);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] <= y3p[i]);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] > y3p[i]);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] < y3p[i]);)
        break;
      default:
        return -1;
      }
    } else {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] == y3p[i]);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] != y3p[i]);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] >= y3p[i]);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] <= y3p[i]);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] > y3p[i]);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] < y3p[i]);)
        break;
      default:
        return -1;
      }
    }

  } else {
    if (o2 == OP_EQ) {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] == y3p[i]);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] != y3p[i]);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] >= y3p[i]);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] <= y3p[i]);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] > y3p[i]);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] < y3p[i]);)
        break;
      default:
        return UNSUPPORTED_OP;
      }
    } else {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] == y3p[i]);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] != y3p[i]);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] >= y3p[i]);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] <= y3p[i]);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] > y3p[i]);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] < y3p[i]);)
        break;
      default:
        return UNSUPPORTED_OP;
      }
    }
  }
  return o;
}

R_xlen_t sum_LLi(const int o1, const int * x1p, R_xlen_t N,
                 const int o2, const int * x2p, R_xlen_t N2,
                 const int o3, const int * x3p, const int y3,
                 int nThread) {
  if (N2 != N) {
    return -1;
  }
  R_xlen_t o = 0;
  if (o1 == OP_EQ) {
    if (o2 == OP_EQ) {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] == y3);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] != y3);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] >= y3);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] <= y3);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] > y3);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] < y3);)
        break;
      default:
        return -1;
      }
    } else {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] == y3);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] != y3);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] >= y3);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] <= y3);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] > y3);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] < y3);)
        break;
      default:
        return -1;
      }
    }

  } else {
    if (o2 == OP_EQ) {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] == y3);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] != y3);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] >= y3);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] <= y3);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] > y3);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] < y3);)
        break;
      default:
        return -1;
      }
    } else {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] == y3);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] != y3);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] >= y3);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] <= y3);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] > y3);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] < y3);)
        break;
      default:
        return -1;
      }
    }
  }
  return o;
}
R_xlen_t sum_LLD(const int o1, const int * x1p, R_xlen_t N1,
                 const int o2, const int * x2p, R_xlen_t N2,
                 const int o3, const int * x3p, const double * y3p,
                 int nThread) {
  R_xlen_t N = (N1 >= N2) ? N1 : N2;
  if (N != N1 || N != N2) {
    return -1;
  }
  R_xlen_t o = 0;
  if (o1 == OP_EQ) {
    if (o2 == OP_EQ) {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] == y3p[i]);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] != y3p[i]);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] >= y3p[i]);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] <= y3p[i]);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] > y3p[i]);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] < y3p[i]);)
        break;
      default:
        return -1;
      }
    } else {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] == y3p[i]);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] != y3p[i]);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] >= y3p[i]);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] <= y3p[i]);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] > y3p[i]);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] < y3p[i]);)
        break;
      default:
        return -1;
      }
    }

  } else {
    if (o2 == OP_EQ) {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] == y3p[i]);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] != y3p[i]);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] >= y3p[i]);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] <= y3p[i]);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] > y3p[i]);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] < y3p[i]);)
        break;
      default:
        return -1;
      }
    } else {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] == y3p[i]);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] != y3p[i]);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] >= y3p[i]);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] <= y3p[i]);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] > y3p[i]);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] < y3p[i]);)
        break;
      default:
        return -1;
      }
    }
  }
  return o;
}

R_xlen_t sum_LLd(const int o1, const int * x1p, R_xlen_t N,
                 const int o2, const int * x2p, R_xlen_t N2,
                 const int o3, const int * x3p, const double y3,
                 int nThread) {
  if (N2 != N) {
    return -1;
  }
  R_xlen_t o = 0;
  if (o1 == OP_EQ) {
    if (o2 == OP_EQ) {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] == y3);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] != y3);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] >= y3);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] <= y3);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] > y3);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] < y3);)
        break;
      default:
        return -1;
      }
    } else {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] == y3);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] != y3);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] >= y3);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] <= y3);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] > y3);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] < y3);)
        break;
      default:
        return -1;
      }
    }

  } else {
    if (o2 == OP_EQ) {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] == y3);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] != y3);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] >= y3);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] <= y3);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] > y3);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] < y3);)
        break;
      default:
        return -1;
      }
    } else {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] == y3);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] != y3);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] >= y3);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] <= y3);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] > y3);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] < y3);)
        break;
      default:
        return -1;
      }
    }
  }
  return o;
}

R_xlen_t sum_LLID(const int o1, const int * x1p, R_xlen_t N1,
                  const int o2, const int * x2p, R_xlen_t N2,
                  const int o3, const int * x3p, const double * y3p,
                  int nThread) {
  R_xlen_t N = (N1 >= N2) ? N1 : N2;
  if (N != N1 || N != N2) {
    return -1;
  }
  R_xlen_t o = 0;
  if (o1 == OP_EQ) {
    if (o2 == OP_EQ) {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] == y3p[i]);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] != y3p[i]);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] >= y3p[i]);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] <= y3p[i]);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] > y3p[i]);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] < y3p[i]);)
        break;
      default:
        return -1;
      }
    } else {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] == y3p[i]);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] != y3p[i]);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] >= y3p[i]);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] <= y3p[i]);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] > y3p[i]);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] < y3p[i]);)
        break;
      default:
        return -1;
      }
    }

  } else {
    if (o2 == OP_EQ) {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] == y3p[i]);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] != y3p[i]);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] >= y3p[i]);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] <= y3p[i]);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] > y3p[i]);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] < y3p[i]);)
        break;
      default:
        return -1;
      }
    } else {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] == y3p[i]);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] != y3p[i]);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] >= y3p[i]);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] <= y3p[i]);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] > y3p[i]);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] < y3p[i]);)
        break;
      default:
        return -1;
      }
    }
  }
  return o;
}
R_xlen_t sum_LLId(const int o1, const int * x1p, R_xlen_t N,
                  const int o2, const int * x2p, R_xlen_t N2,
                  const int o3, const int * x3p, const double y3,
                  int nThread) {
  if (N2 != N) {
    return -1;
  }
  int int_safety = dbl2int(y3);
  switch(int_safety) {
  case 0:
    switch(o3) {
    case OP_IN:
    case OP_EQ:
      return 0;
    case OP_NE:
    case OP_NI:
      return sum_LLI(o1, x1p, N,
                     o2, x2p, N,
                     o3, x3p, x3p,  // same pointer
                     nThread);
    }
    break;
  case 2:
    return 0; // NA never ok
  case 1:
    return sum_LLi(o1, x1p, N, o2, x2p, N, o3, x3p, (const int)y3, nThread);
    break;
  }
  R_xlen_t o = 0;
  if (o1 == OP_EQ) {
    if (o2 == OP_EQ) {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] == y3);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] != y3);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] >= y3);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] <= y3);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] > y3);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += x1p[i] && x2p[i] && (x3p[i] < y3);)
        break;
      default:
        return -1;
      }
    } else {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] == y3);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] != y3);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] >= y3);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] <= y3);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] > y3);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += x1p[i] && !x2p[i] && (x3p[i] < y3);)
        break;
      default:
        return -1;
      }
    }

  } else {
    if (o2 == OP_EQ) {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] == y3);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] != y3);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] >= y3);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] <= y3);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] > y3);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += !x1p[i] && x2p[i] && (x3p[i] < y3);)
        break;
      default:
        return -1;
      }
    } else {
      switch(o3) {
      case OP_EQ:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] == y3);)
        break;
      case OP_NE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] != y3);)
        break;
      case OP_GE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] >= y3);)
        break;
      case OP_LE:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] <= y3);)
        break;
      case OP_GT:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] > y3);)
        break;
      case OP_LT:
        FORLOOP_redsum(o += !x1p[i] && !x2p[i] && (x3p[i] < y3);)
        break;
      default:
        return -1;
      }
    }
  }
  return o;
}

/*
 * case OP_NE:
 * case OP_EQ:
 * case OP_GE:
 * case OP_LE:
 * case OP_GT:
 * case OP_LT::
 */

static R_xlen_t asum_DD_DD(const int o1, const double * x1p, R_xlen_t N, const double * y1p, R_xlen_t M1,
                           const int o2, const double * x2p, R_xlen_t N2, const double * y2p, R_xlen_t M2,
                           int nThread) {
  if (N2 != N) {
    return -1;
  }
  R_xlen_t o = 0;

  switch(o1) {
  case OP_FALSE:
    return 0;
  case OP_TRUE:
    return sum_dd(o2, x2p, N, y2p, M2, nThread);
  case OP_NE:
    if (M1 != 1 && M1 != N) {
      return RHS_BAD_LEN;
    } else {
      if (M1 == 1) {
        const double y1_0 = y1p[0];
        if (ISNAN(y1_0) || y1_0 == R_NegInf) {
          return 0;
        }
        switch(o2) {
        case OP_NE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] != y1_0 && x2p[i] != y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] != y1_0 && x2p[i] != y2p[i];)
            }
          }
          break;
        case OP_EQ:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] != y1_0 && x2p[i] == y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] != y1_0 && x2p[i] == y2p[i];)
            }
          }
          break;
        case OP_GE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] != y1_0 && x2p[i] >= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] != y1_0 && x2p[i] >= y2p[i];)
            }
          }
          break;
        case OP_LE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] != y1_0 && x2p[i] <= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] != y1_0 && x2p[i] <= y2p[i];)
            }
          }
          break;
        case OP_GT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] != y1_0 && x2p[i] > y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] != y1_0 && x2p[i] > y2p[i];)
            }
          }
          break;
        case OP_LT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] != y1_0 && x2p[i] < y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] != y1_0 && x2p[i] < y2p[i];)
            }
          }
          break;
        case OP_BW:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y1_0) {
              return 0;
            }
            if (y2_0 == y1_0) {
              return asum_DD_DD(OP_EQ, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] != y1_0 && x2p[i] <= y2_1;)
          }
          break;
        case OP_BO:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            if (ISNAN(y2p[0])) {
              // -Inf > -Inf
              return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (ISNAN(y2p[1])) {
              return asum_DD_DD(OP_GT, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            const double y2_0 = y2p[0];
            const double y2_1 = y2p[1];
            if (y2_0 >= y2_1) {
              return 0;
            }
            FORLOOP_redsum(o += x1p[i] != y1_0 && x2p[i] < y2_1;)
          }
          break;
        case OP_BC:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (y2_0 == R_NegInf) {
              // goto
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_LE, x2p, N2, &y2p[1], 1,
                                nThread);
            }
            if (y2_1 == R_PosInf) {
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_GE, x2p, N2, y2p, 1,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] != y1_0 && x2p[i] >= y2_1;)
          }
          break;
        }
      } else {
        switch(o2) {
        case OP_NE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] != y1p[i] && x2p[i] != y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] != y1p[i] && x2p[i] != y2p[i];)
            }
          }
          break;
        case OP_EQ:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] != y1p[i] && x2p[i] == y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] != y1p[i] && x2p[i] == y2p[i];)
            }
          }
          break;
        case OP_GE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] != y1p[i] && x2p[i] >= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] != y1p[i] && x2p[i] >= y2p[i];)
            }
          }
          break;
        case OP_LE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] != y1p[i] && x2p[i] <= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] != y1p[i] && x2p[i] <= y2p[i];)
            }
          }
          break;
        case OP_GT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] != y1p[i] && x2p[i] > y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] != y1p[i] && x2p[i] > y2p[i];)
            }
          }
          break;
        case OP_LT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] != y1p[i] && x2p[i] < y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] != y1p[i] && x2p[i] < y2p[i];)
            }
          }
          break;
        case OP_BW:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_EQ, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] != y1p[i] && x2p[i] <= y2_1;)
          }
          break;
        case OP_BO:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            if (ISNAN(y2p[0])) {
              // -Inf > -Inf
              return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (ISNAN(y2p[1])) {
              return asum_DD_DD(OP_GT, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            const double y2_0 = y2p[0];
            const double y2_1 = y2p[1];
            if (y2_0 >= y2_1) {
              return 0;
            }
            FORLOOP_redsum(o += x1p[i] != y1p[i] && x2p[i] < y2_1;)
          }
          break;
        case OP_BC:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (y2_0 == R_NegInf) {
              // goto
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_LE, x2p, N2, &y2p[1], 1,
                                nThread);
            }
            if (y2_1 == R_PosInf) {
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_GE, x2p, N2, y2p, 1,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] != y1p[i] && x2p[i] >= y2_1;)
          }
          break;
        }
      }
    }
    break;
  case OP_EQ:
    if (M1 != 1 && M1 != N) {
      return RHS_BAD_LEN;
    } else {
      if (M1 == 1) {
        const double y1_0 = y1p[0];
        if (ISNAN(y1_0) || y1_0 == R_NegInf) {
          return 0;
        }
        switch(o2) {
        case OP_NE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] == y1_0 && x2p[i] != y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] == y1_0 && x2p[i] != y2p[i];)
            }
          }
          break;
        case OP_EQ:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] == y1_0 && x2p[i] == y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] == y1_0 && x2p[i] == y2p[i];)
            }
          }
          break;
        case OP_GE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] == y1_0 && x2p[i] >= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] == y1_0 && x2p[i] >= y2p[i];)
            }
          }
          break;
        case OP_LE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] == y1_0 && x2p[i] <= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] == y1_0 && x2p[i] <= y2p[i];)
            }
          }
          break;
        case OP_GT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] == y1_0 && x2p[i] > y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] == y1_0 && x2p[i] > y2p[i];)
            }
          }
          break;
        case OP_LT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] == y1_0 && x2p[i] < y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] == y1_0 && x2p[i] < y2p[i];)
            }
          }
          break;
        case OP_BW:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y1_0) {
              return 0;
            }
            if (y2_0 == y1_0) {
              return asum_DD_DD(OP_EQ, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] == y1_0 && x2p[i] <= y2_1;)
          }
          break;
        case OP_BO:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            if (ISNAN(y2p[0])) {
              // -Inf > -Inf
              return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (ISNAN(y2p[1])) {
              return asum_DD_DD(OP_GT, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            const double y2_0 = y2p[0];
            const double y2_1 = y2p[1];
            if (y2_0 >= y2_1) {
              return 0;
            }
            FORLOOP_redsum(o += x1p[i] == y1_0 && x2p[i] < y2_1;)
          }
          break;
        case OP_BC:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (y2_0 == R_NegInf) {
              // goto
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_LE, x2p, N2, &y2p[1], 1,
                                nThread);
            }
            if (y2_1 == R_PosInf) {
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_GE, x2p, N2, y2p, 1,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] == y1_0 && x2p[i] >= y2_1;)
          }
          break;
        }
      } else {
        switch(o2) {
        case OP_NE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] == y1p[i] && x2p[i] != y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] == y1p[i] && x2p[i] != y2p[i];)
            }
          }
          break;
        case OP_EQ:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] == y1p[i] && x2p[i] == y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] == y1p[i] && x2p[i] == y2p[i];)
            }
          }
          break;
        case OP_GE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] == y1p[i] && x2p[i] >= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] == y1p[i] && x2p[i] >= y2p[i];)
            }
          }
          break;
        case OP_LE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] == y1p[i] && x2p[i] <= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] == y1p[i] && x2p[i] <= y2p[i];)
            }
          }
          break;
        case OP_GT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] == y1p[i] && x2p[i] > y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] == y1p[i] && x2p[i] > y2p[i];)
            }
          }
          break;
        case OP_LT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] == y1p[i] && x2p[i] < y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] == y1p[i] && x2p[i] < y2p[i];)
            }
          }
          break;
        case OP_BW:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_EQ, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] == y1p[i] && x2p[i] <= y2_1;)
          }
          break;
        case OP_BO:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            if (ISNAN(y2p[0])) {
              // -Inf > -Inf
              return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (ISNAN(y2p[1])) {
              return asum_DD_DD(OP_GT, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            const double y2_0 = y2p[0];
            const double y2_1 = y2p[1];
            if (y2_0 >= y2_1) {
              return 0;
            }
            FORLOOP_redsum(o += x1p[i] == y1p[i] && x2p[i] < y2_1;)
          }
          break;
        case OP_BC:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (y2_0 == R_NegInf) {
              // goto
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_LE, x2p, N2, &y2p[1], 1,
                                nThread);
            }
            if (y2_1 == R_PosInf) {
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_GE, x2p, N2, y2p, 1,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] == y1p[i] && x2p[i] >= y2_1;)
          }
          break;
        }
      }
    }
    break;
  case OP_GE:
    if (M1 != 1 && M1 != N) {
      return RHS_BAD_LEN;
    } else {
      if (M1 == 1) {
        const double y1_0 = y1p[0];
        if (ISNAN(y1_0) || y1_0 == R_NegInf) {
          return 0;
        }
        switch(o2) {
        case OP_NE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] >= y1_0 && x2p[i] != y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] >= y1_0 && x2p[i] != y2p[i];)
            }
          }
          break;
        case OP_EQ:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] >= y1_0 && x2p[i] == y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] >= y1_0 && x2p[i] == y2p[i];)
            }
          }
          break;
        case OP_GE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] >= y1_0 && x2p[i] >= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] >= y1_0 && x2p[i] >= y2p[i];)
            }
          }
          break;
        case OP_LE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] >= y1_0 && x2p[i] <= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] >= y1_0 && x2p[i] <= y2p[i];)
            }
          }
          break;
        case OP_GT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] >= y1_0 && x2p[i] > y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] >= y1_0 && x2p[i] > y2p[i];)
            }
          }
          break;
        case OP_LT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] >= y1_0 && x2p[i] < y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] >= y1_0 && x2p[i] < y2p[i];)
            }
          }
          break;
        case OP_BW:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y1_0) {
              return 0;
            }
            if (y2_0 == y1_0) {
              return asum_DD_DD(OP_EQ, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] >= y1_0 && x2p[i] <= y2_1;)
          }
          break;
        case OP_BO:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            if (ISNAN(y2p[0])) {
              // -Inf > -Inf
              return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (ISNAN(y2p[1])) {
              return asum_DD_DD(OP_GT, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            const double y2_0 = y2p[0];
            const double y2_1 = y2p[1];
            if (y2_0 >= y2_1) {
              return 0;
            }
            FORLOOP_redsum(o += x1p[i] >= y1_0 && x2p[i] < y2_1;)
          }
          break;
        case OP_BC:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (y2_0 == R_NegInf) {
              // goto
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_LE, x2p, N2, &y2p[1], 1,
                                nThread);
            }
            if (y2_1 == R_PosInf) {
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_GE, x2p, N2, y2p, 1,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] >= y1_0 && x2p[i] >= y2_1;)
          }
          break;
        }
      } else {
        switch(o2) {
        case OP_NE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] >= y1p[i] && x2p[i] != y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] >= y1p[i] && x2p[i] != y2p[i];)
            }
          }
          break;
        case OP_EQ:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] >= y1p[i] && x2p[i] == y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] >= y1p[i] && x2p[i] == y2p[i];)
            }
          }
          break;
        case OP_GE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] >= y1p[i] && x2p[i] >= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] >= y1p[i] && x2p[i] >= y2p[i];)
            }
          }
          break;
        case OP_LE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] >= y1p[i] && x2p[i] <= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] >= y1p[i] && x2p[i] <= y2p[i];)
            }
          }
          break;
        case OP_GT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] >= y1p[i] && x2p[i] > y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] >= y1p[i] && x2p[i] > y2p[i];)
            }
          }
          break;
        case OP_LT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] >= y1p[i] && x2p[i] < y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] >= y1p[i] && x2p[i] < y2p[i];)
            }
          }
          break;
        case OP_BW:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_EQ, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] >= y1p[i] && x2p[i] <= y2_1;)
          }
          break;
        case OP_BO:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            if (ISNAN(y2p[0])) {
              // -Inf > -Inf
              return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (ISNAN(y2p[1])) {
              return asum_DD_DD(OP_GT, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            const double y2_0 = y2p[0];
            const double y2_1 = y2p[1];
            if (y2_0 >= y2_1) {
              return 0;
            }
            FORLOOP_redsum(o += x1p[i] >= y1p[i] && x2p[i] < y2_1;)
          }
          break;
        case OP_BC:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (y2_0 == R_NegInf) {
              // goto
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_LE, x2p, N2, &y2p[1], 1,
                                nThread);
            }
            if (y2_1 == R_PosInf) {
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_GE, x2p, N2, y2p, 1,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] >= y1p[i] && x2p[i] >= y2_1;)
          }
          break;
        }
      }
    }
    break;
  case OP_LE:
    if (M1 != 1 && M1 != N) {
      return RHS_BAD_LEN;
    } else {
      if (M1 == 1) {
        const double y1_0 = y1p[0];
        if (ISNAN(y1_0) || y1_0 == R_NegInf) {
          return 0;
        }
        switch(o2) {
        case OP_NE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x2p[i] != y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x2p[i] != y2p[i];)
            }
          }
          break;
        case OP_EQ:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x2p[i] == y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x2p[i] == y2p[i];)
            }
          }
          break;
        case OP_GE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x2p[i] >= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x2p[i] >= y2p[i];)
            }
          }
          break;
        case OP_LE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x2p[i] <= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x2p[i] <= y2p[i];)
            }
          }
          break;
        case OP_GT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x2p[i] > y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x2p[i] > y2p[i];)
            }
          }
          break;
        case OP_LT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x2p[i] < y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x2p[i] < y2p[i];)
            }
          }
          break;
        case OP_BW:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y1_0) {
              return 0;
            }
            if (y2_0 == y1_0) {
              return asum_DD_DD(OP_EQ, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] <= y1_0 && x2p[i] <= y2_1;)
          }
          break;
        case OP_BO:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            if (ISNAN(y2p[0])) {
              // -Inf > -Inf
              return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (ISNAN(y2p[1])) {
              return asum_DD_DD(OP_GT, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            const double y2_0 = y2p[0];
            const double y2_1 = y2p[1];
            if (y2_0 >= y2_1) {
              return 0;
            }
            FORLOOP_redsum(o += x1p[i] <= y1_0 && x2p[i] < y2_1;)
          }
          break;
        case OP_BC:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (y2_0 == R_NegInf) {
              // goto
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_LE, x2p, N2, &y2p[1], 1,
                                nThread);
            }
            if (y2_1 == R_PosInf) {
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_GE, x2p, N2, y2p, 1,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] <= y1_0 && x2p[i] >= y2_1;)
          }
          break;
        }
      } else {
        switch(o2) {
        case OP_NE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1p[i] && x2p[i] != y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1p[i] && x2p[i] != y2p[i];)
            }
          }
          break;
        case OP_EQ:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1p[i] && x2p[i] == y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1p[i] && x2p[i] == y2p[i];)
            }
          }
          break;
        case OP_GE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1p[i] && x2p[i] >= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1p[i] && x2p[i] >= y2p[i];)
            }
          }
          break;
        case OP_LE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1p[i] && x2p[i] <= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1p[i] && x2p[i] <= y2p[i];)
            }
          }
          break;
        case OP_GT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1p[i] && x2p[i] > y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1p[i] && x2p[i] > y2p[i];)
            }
          }
          break;
        case OP_LT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1p[i] && x2p[i] < y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1p[i] && x2p[i] < y2p[i];)
            }
          }
          break;
        case OP_BW:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_EQ, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] <= y1p[i] && x2p[i] <= y2_1;)
          }
          break;
        case OP_BO:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            if (ISNAN(y2p[0])) {
              // -Inf > -Inf
              return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (ISNAN(y2p[1])) {
              return asum_DD_DD(OP_GT, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            const double y2_0 = y2p[0];
            const double y2_1 = y2p[1];
            if (y2_0 >= y2_1) {
              return 0;
            }
            FORLOOP_redsum(o += x1p[i] <= y1p[i] && x2p[i] < y2_1;)
          }
          break;
        case OP_BC:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (y2_0 == R_NegInf) {
              // goto
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_LE, x2p, N2, &y2p[1], 1,
                                nThread);
            }
            if (y2_1 == R_PosInf) {
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_GE, x2p, N2, y2p, 1,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] <= y1p[i] && x2p[i] >= y2_1;)
          }
          break;
        }
      }
    }
    break;
  case OP_GT:
    if (M1 != 1 && M1 != N) {
      return RHS_BAD_LEN;
    } else {
      if (M1 == 1) {
        const double y1_0 = y1p[0];
        if (ISNAN(y1_0) || y1_0 == R_NegInf) {
          return 0;
        }
        switch(o2) {
        case OP_NE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] > y1_0 && x2p[i] != y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] > y1_0 && x2p[i] != y2p[i];)
            }
          }
          break;
        case OP_EQ:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] > y1_0 && x2p[i] == y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] > y1_0 && x2p[i] == y2p[i];)
            }
          }
          break;
        case OP_GE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] > y1_0 && x2p[i] >= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] > y1_0 && x2p[i] >= y2p[i];)
            }
          }
          break;
        case OP_LE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] > y1_0 && x2p[i] <= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] > y1_0 && x2p[i] <= y2p[i];)
            }
          }
          break;
        case OP_GT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] > y1_0 && x2p[i] > y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] > y1_0 && x2p[i] > y2p[i];)
            }
          }
          break;
        case OP_LT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] > y1_0 && x2p[i] < y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] > y1_0 && x2p[i] < y2p[i];)
            }
          }
          break;
        case OP_BW:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y1_0) {
              return 0;
            }
            if (y2_0 == y1_0) {
              return asum_DD_DD(OP_EQ, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] > y1_0 && x2p[i] <= y2_1;)
          }
          break;
        case OP_BO:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            if (ISNAN(y2p[0])) {
              // -Inf > -Inf
              return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (ISNAN(y2p[1])) {
              return asum_DD_DD(OP_GT, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            const double y2_0 = y2p[0];
            const double y2_1 = y2p[1];
            if (y2_0 >= y2_1) {
              return 0;
            }
            FORLOOP_redsum(o += x1p[i] > y1_0 && x2p[i] < y2_1;)
          }
          break;
        case OP_BC:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (y2_0 == R_NegInf) {
              // goto
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_LE, x2p, N2, &y2p[1], 1,
                                nThread);
            }
            if (y2_1 == R_PosInf) {
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_GE, x2p, N2, y2p, 1,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] > y1_0 && x2p[i] >= y2_1;)
          }
          break;
        }
      } else {
        switch(o2) {
        case OP_NE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] > y1p[i] && x2p[i] != y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] > y1p[i] && x2p[i] != y2p[i];)
            }
          }
          break;
        case OP_EQ:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] > y1p[i] && x2p[i] == y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] > y1p[i] && x2p[i] == y2p[i];)
            }
          }
          break;
        case OP_GE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] > y1p[i] && x2p[i] >= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] > y1p[i] && x2p[i] >= y2p[i];)
            }
          }
          break;
        case OP_LE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] > y1p[i] && x2p[i] <= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] > y1p[i] && x2p[i] <= y2p[i];)
            }
          }
          break;
        case OP_GT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] > y1p[i] && x2p[i] > y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] > y1p[i] && x2p[i] > y2p[i];)
            }
          }
          break;
        case OP_LT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] > y1p[i] && x2p[i] < y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] > y1p[i] && x2p[i] < y2p[i];)
            }
          }
          break;
        case OP_BW:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_EQ, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] > y1p[i] && x2p[i] <= y2_1;)
          }
          break;
        case OP_BO:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            if (ISNAN(y2p[0])) {
              // -Inf > -Inf
              return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (ISNAN(y2p[1])) {
              return asum_DD_DD(OP_GT, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            const double y2_0 = y2p[0];
            const double y2_1 = y2p[1];
            if (y2_0 >= y2_1) {
              return 0;
            }
            FORLOOP_redsum(o += x1p[i] > y1p[i] && x2p[i] < y2_1;)
          }
          break;
        case OP_BC:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (y2_0 == R_NegInf) {
              // goto
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_LE, x2p, N2, &y2p[1], 1,
                                nThread);
            }
            if (y2_1 == R_PosInf) {
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_GE, x2p, N2, y2p, 1,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] > y1p[i] && x2p[i] >= y2_1;)
          }
          break;
        }
      }
    }
    break;
  case OP_LT:
    if (M1 != 1 && M1 != N) {
      return RHS_BAD_LEN;
    } else {
      if (M1 == 1) {
        const double y1_0 = y1p[0];
        if (ISNAN(y1_0) || y1_0 == R_NegInf) {
          return 0;
        }
        switch(o2) {
        case OP_NE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] < y1_0 && x2p[i] != y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] < y1_0 && x2p[i] != y2p[i];)
            }
          }
          break;
        case OP_EQ:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] < y1_0 && x2p[i] == y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] < y1_0 && x2p[i] == y2p[i];)
            }
          }
          break;
        case OP_GE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] < y1_0 && x2p[i] >= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] < y1_0 && x2p[i] >= y2p[i];)
            }
          }
          break;
        case OP_LE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] < y1_0 && x2p[i] <= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] < y1_0 && x2p[i] <= y2p[i];)
            }
          }
          break;
        case OP_GT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] < y1_0 && x2p[i] > y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] < y1_0 && x2p[i] > y2p[i];)
            }
          }
          break;
        case OP_LT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] < y1_0 && x2p[i] < y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] < y1_0 && x2p[i] < y2p[i];)
            }
          }
          break;
        case OP_BW:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y1_0) {
              return 0;
            }
            if (y2_0 == y1_0) {
              return asum_DD_DD(OP_EQ, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] < y1_0 && x2p[i] <= y2_1;)
          }
          break;
        case OP_BO:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            if (ISNAN(y2p[0])) {
              // -Inf > -Inf
              return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (ISNAN(y2p[1])) {
              return asum_DD_DD(OP_GT, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            const double y2_0 = y2p[0];
            const double y2_1 = y2p[1];
            if (y2_0 >= y2_1) {
              return 0;
            }
            FORLOOP_redsum(o += x1p[i] < y1_0 && x2p[i] < y2_1;)
          }
          break;
        case OP_BC:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (y2_0 == R_NegInf) {
              // goto
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_LE, x2p, N2, &y2p[1], 1,
                                nThread);
            }
            if (y2_1 == R_PosInf) {
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_GE, x2p, N2, y2p, 1,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] < y1_0 && x2p[i] >= y2_1;)
          }
          break;
        }
      } else {
        switch(o2) {
        case OP_NE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] < y1p[i] && x2p[i] != y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] < y1p[i] && x2p[i] != y2p[i];)
            }
          }
          break;
        case OP_EQ:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] < y1p[i] && x2p[i] == y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] < y1p[i] && x2p[i] == y2p[i];)
            }
          }
          break;
        case OP_GE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] < y1p[i] && x2p[i] >= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] < y1p[i] && x2p[i] >= y2p[i];)
            }
          }
          break;
        case OP_LE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] < y1p[i] && x2p[i] <= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] < y1p[i] && x2p[i] <= y2p[i];)
            }
          }
          break;
        case OP_GT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] < y1p[i] && x2p[i] > y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] < y1p[i] && x2p[i] > y2p[i];)
            }
          }
          break;
        case OP_LT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] < y1p[i] && x2p[i] < y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] < y1p[i] && x2p[i] < y2p[i];)
            }
          }
          break;
        case OP_BW:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_EQ, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] < y1p[i] && x2p[i] <= y2_1;)
          }
          break;
        case OP_BO:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            if (ISNAN(y2p[0])) {
              // -Inf > -Inf
              return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (ISNAN(y2p[1])) {
              return asum_DD_DD(OP_GT, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            const double y2_0 = y2p[0];
            const double y2_1 = y2p[1];
            if (y2_0 >= y2_1) {
              return 0;
            }
            FORLOOP_redsum(o += x1p[i] < y1p[i] && x2p[i] < y2_1;)
          }
          break;
        case OP_BC:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y2_1) {
              return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (y2_0 == R_NegInf) {
              // goto
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_LE, x2p, N2, &y2p[1], 1,
                                nThread);
            }
            if (y2_1 == R_PosInf) {
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_GE, x2p, N2, y2p, 1,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] < y1p[i] && x2p[i] >= y2_1;)
          }
          break;
        }
      }
    }
    break;
  case OP_BW:
    if (M1 != 2) {
      return BW_LEN_NE2;
    } else {
      if (ISNAN(y1p[0]) && ISNAN(y1p[1])) {
        return sum_dd(o2, x2p, N, y2p, M2, nThread);
      }
      const double y1_0 = ISNAN(y1p[0]) ? R_NegInf : y1p[0];
      const double y1_1 = ISNAN(y1p[1]) ? R_PosInf : y1p[1];
      if (y1_0 > y1_1) {
        return 0;
      }
      if (y1_0 == R_NegInf) {
        return asum_DD_DD(OP_GE, x1p, N, y1p, 1,
                          o2, x2p, N2, y2p, M2,
                          nThread);
      }
      if (y1_1 == R_NegInf) {
        return asum_DD_DD(OP_LE, x1p, N, &y1p[1], 1,
                          o2, x2p, N2, y2p, M2,
                          nThread);
      }
      switch(o2) {
      case OP_NE:
        if (M2 != 1 && M2 != N) {
          return RHS_BAD_LEN;
        } else {
          if (M2 == 1) {
            const double y2_0 = y2p[0];
            FORLOOP_redsum(o += x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] != y2_0;)
          } else {
            FORLOOP_redsum(o += x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] != y2p[i];)
          }
        }
        break;
      case OP_EQ:
        if (M2 != 1 && M2 != N) {
          return RHS_BAD_LEN;
        } else {
          if (M2 == 1) {
            const double y2_0 = y2p[0];
            FORLOOP_redsum(o += x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] == y2_0;)
          } else {
            FORLOOP_redsum(o += x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] == y2p[i];)
          }
        }
        break;
      case OP_GE:
        if (M2 != 1 && M2 != N) {
          return RHS_BAD_LEN;
        } else {
          if (M2 == 1) {
            const double y2_0 = y2p[0];
            FORLOOP_redsum(o += x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] >= y2_0;)
          } else {
            FORLOOP_redsum(o += x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] >= y2p[i];)
          }
        }
        break;
      case OP_LE:
        if (M2 != 1 && M2 != N) {
          return RHS_BAD_LEN;
        } else {
          if (M2 == 1) {
            const double y2_0 = y2p[0];
            FORLOOP_redsum(o += x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] <= y2_0;)
          } else {
            FORLOOP_redsum(o += x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] <= y2p[i];)
          }
        }
        break;
      case OP_GT:
        if (M2 != 1 && M2 != N) {
          return RHS_BAD_LEN;
        } else {
          if (M2 == 1) {
            const double y2_0 = y2p[0];
            FORLOOP_redsum(o += x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] > y2_0;)
          } else {
            FORLOOP_redsum(o += x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] > y2p[i];)
          }
        }
        break;
      case OP_LT:
        if (M2 != 1 && M2 != N) {
          return RHS_BAD_LEN;
        } else {
          if (M2 == 1) {
            const double y2_0 = y2p[0];
            FORLOOP_redsum(o += x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] < y2_0;)
          } else {
            FORLOOP_redsum(o += x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] < y2p[i];)
          }
        }
        break;
      case OP_BW:
        if (M2 != 2) {
          return BW_LEN_NE2;
        } else {
          const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
          const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
          if (y2_0 > y1_0) {
            return 0;
          }
          if (y2_0 == y1_0) {
            return asum_DD_DD(OP_EQ, x1p, N, &y1p[0], 1,
                              o2, x2p, N2, y2p, M2,
                              nThread);
          }
          FORLOOP_redsum(o += x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] >= y2_0 && x2p[i] <= y2_1;)
        }
        break;
      case OP_BO:
        if (M2 != 2) {
          return BW_LEN_NE2;
        } else {
          if (ISNAN(y2p[0])) {
            // -Inf > -Inf
            return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                              o2, x2p, N2, y2p, M2,
                              nThread);
          }
          if (ISNAN(y2p[1])) {
            return asum_DD_DD(OP_GT, x1p, N, &y1p[0], 1,
                              o2, x2p, N2, y2p, M2,
                              nThread);
          }
          const double y2_0 = y2p[0];
          const double y2_1 = y2p[1];
          if (y2_0 >= y2_1) {
            return 0;
          }
          FORLOOP_redsum(o += x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] > y2_0 && x2p[i] < y2_1;)
        }
        break;
      case OP_BC:
        if (M2 != 2) {
          return BW_LEN_NE2;
        } else {
          const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
          const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
          if (y2_0 > y2_1) {
            return 0;
          }
          if (y2_0 == y1_1) {
            return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                              o2, x2p, N2, y2p, M2,
                              nThread);
          }
          if (y2_0 == R_NegInf) {
            // goto
            return asum_DD_DD(o1, x1p, N, y1p, M1,
                              OP_LE, x2p, N2, &y2p[1], 1,
                              nThread);
          }
          if (y2_1 == R_PosInf) {
            return asum_DD_DD(o1, x1p, N, y1p, M1,
                              OP_GE, x2p, N2, y2p, 1,
                              nThread);
          }
          FORLOOP_redsum(o += x1p[i] >= y1_0 && x1p[i] <= y1_1 && x2p[i] <= y2_0 && x2p[i] >= y2_1;)
        }
        break;
      }
    }
    break;
  case OP_BO:
    if (M1 != 2) {
      return BW_LEN_NE2;
    } else {
      if (ISNAN(y1p[0]) && ISNAN(y1p[1])) {
        return UNSUPPORTED_NA;
      }
      if (ISNAN(y1p[0])) {
        return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                          o2, x2p, N2, y2p, M2,
                          nThread);
      }
      if (ISNAN(y1p[1])) {
        return asum_DD_DD(OP_GT, x1p, N, &y1p[1], 1,
                          o2, x2p, N2, y2p, M2,
                          nThread);
      }
      const double y1_0 = y1p[0];
      const double y1_1 = y1p[1];
      switch(o2) {
      case OP_NE:
        if (M2 != 1 && M2 != N) {
          return RHS_BAD_LEN;
        } else {
          if (M2 == 1) {
            const double y2_0 = y2p[0];
            FORLOOP_redsum(o += x1p[i] > y1_0 && x1p[i] < y1_1 && x2p[i] != y2_0;)
          } else {
            FORLOOP_redsum(o += x1p[i] > y1_0 && x1p[i] < y1_1 && x2p[i] != y2p[i];)
          }
        }
        break;
      case OP_EQ:
        if (M2 != 1 && M2 != N) {
          return RHS_BAD_LEN;
        } else {
          if (M2 == 1) {
            const double y2_0 = y2p[0];
            FORLOOP_redsum(o += x1p[i] > y1_0 && x1p[i] < y1_1 && x2p[i] == y2_0;)
          } else {
            FORLOOP_redsum(o += x1p[i] > y1_0 && x1p[i] < y1_1 && x2p[i] == y2p[i];)
          }
        }
        break;
      case OP_GE:
        if (M2 != 1 && M2 != N) {
          return RHS_BAD_LEN;
        } else {
          if (M2 == 1) {
            const double y2_0 = y2p[0];
            FORLOOP_redsum(o += x1p[i] > y1_0 && x1p[i] < y1_1 && x2p[i] >= y2_0;)
          } else {
            FORLOOP_redsum(o += x1p[i] > y1_0 && x1p[i] < y1_1 && x2p[i] >= y2p[i];)
          }
        }
        break;
      case OP_LE:
        if (M2 != 1 && M2 != N) {
          return RHS_BAD_LEN;
        } else {
          if (M2 == 1) {
            const double y2_0 = y2p[0];
            FORLOOP_redsum(o += x1p[i] > y1_0 && x1p[i] < y1_1 && x2p[i] <= y2_0;)
          } else {
            FORLOOP_redsum(o += x1p[i] > y1_0 && x1p[i] < y1_1 && x2p[i] <= y2p[i];)
          }
        }
        break;
      case OP_GT:
        if (M2 != 1 && M2 != N) {
          return RHS_BAD_LEN;
        } else {
          if (M2 == 1) {
            const double y2_0 = y2p[0];
            FORLOOP_redsum(o += x1p[i] > y1_0 && x1p[i] < y1_1 && x2p[i] > y2_0;)
          } else {
            FORLOOP_redsum(o += x1p[i] > y1_0 && x1p[i] < y1_1 && x2p[i] > y2p[i];)
          }
        }
        break;
      case OP_LT:
        if (M2 != 1 && M2 != N) {
          return RHS_BAD_LEN;
        } else {
          if (M2 == 1) {
            const double y2_0 = y2p[0];
            FORLOOP_redsum(o += x1p[i] > y1_0 && x1p[i] < y1_1 && x2p[i] < y2_0;)
          } else {
            FORLOOP_redsum(o += x1p[i] > y1_0 && x1p[i] < y1_1 && x2p[i] < y2p[i];)
          }
        }
        break;
      case OP_BW:
        if (M2 != 2) {
          return BW_LEN_NE2;
        } else {
          const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
          const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
          if (y2_0 > y1_0) {
            return 0;
          }
          if (y2_0 == y1_0) {
            return asum_DD_DD(OP_EQ, x1p, N, &y1p[0], 1,
                              o2, x2p, N2, y2p, M2,
                              nThread);
          }
          FORLOOP_redsum(o += x1p[i] > y1_0 && x1p[i] < y1_1 && x2p[i] >= y2_0 && x2p[i] <= y2_1;)
        }
        break;
      case OP_BO:
        if (M2 != 2) {
          return BW_LEN_NE2;
        } else {
          if (ISNAN(y2p[0])) {
            // -Inf > -Inf
            return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                              o2, x2p, N2, y2p, M2,
                              nThread);
          }
          if (ISNAN(y2p[1])) {
            return asum_DD_DD(OP_GT, x1p, N, &y1p[0], 1,
                              o2, x2p, N2, y2p, M2,
                              nThread);
          }
          const double y2_0 = y2p[0];
          const double y2_1 = y2p[1];
          if (y2_0 >= y2_1) {
            return 0;
          }
          FORLOOP_redsum(o += x1p[i] > y1_0 && x1p[i] < y1_1 && x2p[i] > y2_0 && x2p[i] < y2_1;)
        }
        break;
      case OP_BC:
        if (M2 != 2) {
          return BW_LEN_NE2;
        } else {
          const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
          const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
          if (y2_0 > y2_1) {
            return 0;
          }
          if (y2_0 == y1_1) {
            return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                              o2, x2p, N2, y2p, M2,
                              nThread);
          }
          if (y2_0 == R_NegInf) {
            // goto
            return asum_DD_DD(o1, x1p, N, y1p, M1,
                              OP_LE, x2p, N2, &y2p[1], 1,
                              nThread);
          }
          if (y2_1 == R_PosInf) {
            return asum_DD_DD(o1, x1p, N, y1p, M1,
                              OP_GE, x2p, N2, y2p, 1,
                              nThread);
          }
          FORLOOP_redsum(o += x1p[i] > y1_0 && x1p[i] < y1_1 && x2p[i] <= y2_0 && x2p[i] >= y2_1;)
        }
        break;
      }
      break;
    case OP_BC:
      if (M1 != 2) {
        return BW_LEN_NE2;
      } else {
        if (ISNAN(y1p[0]) && ISNAN(y2p[0])) {
          return UNSUPPORTED_NA;
        }
        const double y1_0 = ISNAN(y1p[0]) ? R_NegInf : y1p[0];
        const double y1_1 = ISNAN(y1p[1]) ? R_PosInf : y1p[1];
        if (y1_0 > y1_1) {
          return 0;
        }
        if (y1_0 == y1_1) {
          return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                            o2, x2p, N2, y2p, M2,
                            nThread);
        }
        if (y1_0 == R_NegInf) {
          // goto
          return asum_DD_DD(OP_LE, x1p, N, &y1p[1], 1,
                            o2, x2p, N2, y2p, M2,
                            nThread);
        }
        if (y1_1 == R_PosInf) {
          return asum_DD_DD(o1, x1p, N, y1p, M1,
                            o2, x2p, N2, y2p, M2,
                            nThread);
        }
        switch(o2) {
        case OP_NE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x1p[i] >= y1_1 && x2p[i] != y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x1p[i] >= y1_1 && x2p[i] != y2p[i];)
            }
          }
          break;
        case OP_EQ:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x1p[i] >= y1_1 && x2p[i] == y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x1p[i] >= y1_1 && x2p[i] == y2p[i];)
            }
          }
          break;
        case OP_GE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x1p[i] >= y1_1 && x2p[i] >= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x1p[i] >= y1_1 && x2p[i] >= y2p[i];)
            }
          }
          break;
        case OP_LE:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x1p[i] >= y1_1 && x2p[i] <= y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x1p[i] >= y1_1 && x2p[i] <= y2p[i];)
            }
          }
          break;
        case OP_GT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x1p[i] >= y1_1 && x2p[i] > y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x1p[i] >= y1_1 && x2p[i] > y2p[i];)
            }
          }
          break;
        case OP_LT:
          if (M2 != 1 && M2 != N) {
            return RHS_BAD_LEN;
          } else {
            if (M2 == 1) {
              const double y2_0 = y2p[0];
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x1p[i] >= y1_1 && x2p[i] < y2_0;)
            } else {
              FORLOOP_redsum(o += x1p[i] <= y1_0 && x1p[i] >= y1_1 && x2p[i] < y2p[i];)
            }
          }
          break;
        case OP_BW:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y1_0) {
              return 0;
            }
            if (y2_0 == y1_0) {
              return asum_DD_DD(OP_EQ, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] <= y1_0 && x1p[i] >= y1_1 && x2p[i] >= y2_0 && x2p[i] <= y2_1;)
          }
          break;
        case OP_BO:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            if (ISNAN(y2p[0])) {
              // -Inf > -Inf
              return asum_DD_DD(OP_LT, x1p, N, &y1p[1], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (ISNAN(y2p[1])) {
              return asum_DD_DD(OP_GT, x1p, N, &y1p[0], 1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            const double y2_0 = y2p[0];
            const double y2_1 = y2p[1];
            if (y2_0 >= y2_1) {
              return 0;
            }
            FORLOOP_redsum(o += x1p[i] <= y1_0 && x1p[i] >= y1_1 && x2p[i] > y2_0 && x2p[i] < y2_1;)
          }
          break;
        case OP_BC:
          if (M2 != 2) {
            return BW_LEN_NE2;
          } else {
            const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
            const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
            if (y2_0 > y2_1) {
              return 0;
            }
            if (y2_0 == y1_1) {
              return asum_DD_DD(OP_TRUE, x1p, N, x1p, M1,
                                o2, x2p, N2, y2p, M2,
                                nThread);
            }
            if (y2_0 == R_NegInf) {
              // goto
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_LE, x2p, N2, &y2p[1], 1,
                                nThread);
            }
            if (y2_1 == R_PosInf) {
              return asum_DD_DD(o1, x1p, N, y1p, M1,
                                OP_GE, x2p, N2, y2p, 1,
                                nThread);
            }
            FORLOOP_redsum(o += x1p[i] <= y1_0 && x1p[i] >= y1_1 && x2p[i] <= y2_0 && x2p[i] >= y2_1;)
          }
          break;
        default:
          return UNSUPPORTED_OP;
        }
      }
    }
  }
  return o;
}


static R_xlen_t asum_ID_DD_DD(const int o1, const int * x1p, R_xlen_t N, const double * y1p, R_xlen_t M1,
                              const int o2, const double * x2p, R_xlen_t N2, const double * y2p, R_xlen_t M2,
                              const int o3, const double * x3p, R_xlen_t N3, const double * y3p, R_xlen_t M3,
                              int nThread) {
  if (N2 != N || N3 != N) {
    return -1;
  }
  R_xlen_t o = 0;
  switch(o1) {
  case OP_NE:
  case OP_EQ:
  case OP_GE:
  case OP_LE:
  case OP_GT:
  case OP_LT:
  case OP_BW:
  case OP_BO:
  case OP_BC:
    if (M2 != 2) {
      return BW_LEN_NE2;
    } else {
      const double y1_0 = ISNAN(y1p[0]) ? R_NegInf : y1p[0];
      const double y1_1 = ISNAN(y1p[1]) ? R_PosInf : y1p[1];
      if (y1_0 > y1_1) {
        return 0;
      }
      if (y1_0 == y1_1) {
        return asum_DD_DD(o2, x2p, N2, y2p, M2,
                          o3, x3p, N3, y3p, M3,
                          nThread);
      }
      if (y1_0 == R_NegInf) {
        // goto
        return asum_ID_DD_DD(OP_LE, x1p, N, &y1p[1], 1,
                             o2, x2p, N2, y2p, M2,
                             o3, x3p, N3, y3p, M3,
                             nThread);
      }
      if (y1_1 == R_PosInf) {
        return asum_ID_DD_DD(o1, x1p, N, y1p, M1,
                             o2, x2p, N2, y2p, M2,
                             o3, x3p, N3, y3p, M3,
                             nThread);
      }
      switch(o2) {
      case OP_NE:
      case OP_EQ:
      case OP_GE:
      case OP_LE:
      case OP_GT:
      case OP_LT:
      case OP_BW:
      case OP_BO:
      case OP_BC:
        if (M2 != 2) {
          return BW_LEN_NE2;
        } else {
          const double y2_0 = ISNAN(y2p[0]) ? R_NegInf : y2p[0];
          const double y2_1 = ISNAN(y2p[1]) ? R_PosInf : y2p[1];
          if (y2_0 > y2_1) {
            return 0;
          }
          if (y2_0 == y2_1) {
            return asum_ID_DD_DD(o1, x1p, N, y1p, M1,
                                 o2, x2p, N2, y2p, M2,
                                 o3, x3p, N3, y3p, M3,
                                 nThread);
          }
          if (y2_0 == R_NegInf) {
            // goto
            return asum_ID_DD_DD(o1, x1p, N, y1p, M1,
                                 OP_LE, x3p, N3, &y3p[1], 1,
                                 o3, x3p, N3, y3p, M3,
                                 nThread);
          }
          if (y2_1 == R_PosInf) {
            return asum_ID_DD_DD(o1, x1p, N, y1p, M1,
                                 OP_GE, x2p, N2, &y2p[0], 1,
                                 o3, x3p, N3, y3p, M3,
                                 nThread);
          }
          switch(o3) {
          case OP_NE:
          case OP_EQ:
          case OP_GE:
          case OP_LE:
          case OP_GT:
          case OP_LT:
          case OP_BW:
          case OP_BO:
          case OP_BC:
            if (M3 != 2) {
              return BW_LEN_NE2;
            } else {
              const double y3_0 = ISNAN(y3p[0]) ? R_NegInf : y3p[0];
              const double y3_1 = ISNAN(y3p[1]) ? R_PosInf : y3p[1];
              if (y3_0 > y3_1) {
                return 0;
              }
              if (y3_0 == y3_1) {
                return asum_ID_DD_DD(o1, x1p, N, y1p, M1,
                                     o2, x2p, N2, y2p, M2,
                                     OP_EQ, x3p, N3, x3p, N3,
                                     nThread);
              }
              if (y3_0 == R_NegInf) {
                // goto
                return asum_ID_DD_DD(o1, x1p, N, y1p, M1,
                                     o2, x2p, N2, y2p, M2,
                                     OP_LE, x3p, N3, &y3p[1], 1,
                                     nThread);
              }
              if (y3_1 == R_PosInf) {
                return asum_ID_DD_DD(o1, x1p, N, y1p, M1,
                                     o2, x2p, N2, y2p, M2,
                                     OP_GE, x3p, N3, &y3p[0], 1,
                                     nThread);
              }
              FORLOOP_redsum(
                o +=
                  x1p[i] <= y1_0 && x1p[i] >= y1_1 &&
                  x2p[i] <= y2_0 && y2p[i] >= y2_1 &&
                  x3p[i] <= y3_0 && x3p[i] >= y3_1;
              )

            }
          }
        }
      }
    }

  }
  return o;
}

static R_xlen_t osum_ID_DD_DD(const int o1, const int * x1p, R_xlen_t N, const double * y1p, R_xlen_t M1,
                              const int o2, const double * x2p, R_xlen_t N2, const double * y2p, R_xlen_t M2,
                              const int o3, const double * x3p, R_xlen_t N3, const double * y3p, R_xlen_t M3,
                              int nThread) {
  return -1;
}

R_xlen_t Sum3I__(const int o1,
                 const int * x1p,
                 R_xlen_t N,
                 SEXP yy1,
                 const int o2, SEXP xx2, SEXP yy2,
                 const int o3, SEXP xx3, SEXP yy3,
                 bool And,
                 int nThread) {

  switch(TYPEOF(yy1)) {
  case INTSXP:
  case REALSXP:
    switch(TYPEOF(xx2)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
      switch(TYPEOF(yy2)) {
      case INTSXP:
      case REALSXP:
        switch(TYPEOF(xx3)) {
        case INTSXP:
        case REALSXP:
          switch(TYPEOF(yy3)) {
          case INTSXP:
          case REALSXP:
            return
            And ?
            asum_ID_DD_DD(o1, x1p, N, REAL(yy1), xlength(yy1),
                          o2, REAL(xx2), xlength(xx2), REAL(yy2), xlength(yy2),
                          o3, REAL(xx3), xlength(xx3), REAL(yy3), xlength(yy3),
                          nThread) :
            osum_ID_DD_DD(o1, x1p, N, REAL(yy2), xlength(yy2),
                          o2, REAL(xx2), xlength(xx2), REAL(yy2), xlength(yy2),
                          o3, REAL(xx3), xlength(xx3), REAL(yy3), xlength(yy3),
                          nThread);
          }
        }
      }
    }
  }
  return -1;
}

R_xlen_t sum3x(SEXP oo1, SEXP xx1, SEXP yy1,
               SEXP oo2, SEXP xx2, SEXP yy2,
               SEXP oo3, SEXP xx3, SEXP yy3,
               SEXP AAnd,
               SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  const bool And = asLogical(AAnd);
  const int o1 = sex2op(oo1);
  const int o2 = sex2op(oo2);
  const int o3 = sex2op(oo3);
  switch(TYPEOF(xx1)) {
  case LGLSXP:
    switch(TYPEOF(xx2)) {
    case LGLSXP:
      switch(TYPEOF(xx3)) {
      case LGLSXP:
        return sum_lll(o1, LOGICAL(xx1), xlength(xx1),
                       o2, LOGICAL(xx2), xlength(xx2),
                       o3, LOGICAL(xx3), xlength(xx3),
                       nThread);
      case INTSXP:
        switch(TYPEOF(yy3)) {
        case INTSXP:
          if (xlength(yy3) == xlength(xx3)) {
            return sum_LLI(o1, LOGICAL(xx1), xlength(xx1),
                           o2, LOGICAL(xx2), xlength(xx2),
                           o3, INTEGER(xx3), INTEGER(yy3),
                           nThread);
          } else {
            return sum_LLi(o1, LOGICAL(xx1), xlength(xx1),
                           o2, LOGICAL(xx2), xlength(xx2),
                           o3, INTEGER(xx3), asInteger(yy3),
                           nThread);
          }
          break;
        case REALSXP:
          if (xlength(yy3) == xlength(xx3)) {
            return sum_LLID(o1, LOGICAL(xx1), xlength(xx1),
                            o2, LOGICAL(xx2), xlength(xx2),
                            o3, INTEGER(xx3), REAL(yy3),
                            nThread);
          } else {
            return sum_LLId(o1, LOGICAL(xx1), xlength(xx1),
                            o2, LOGICAL(xx2), xlength(xx2),
                            o3, INTEGER(xx3), asReal(yy3),
                            nThread);
          }
          break;
        }
        break;
      case REALSXP:
        switch(TYPEOF(xx3)) {
        case INTSXP:
          switch(TYPEOF(yy2)) {
          case INTSXP:
            return -1;
          }
          switch(TYPEOF(yy3)) {
          case INTSXP:
            break;
          case REALSXP:
            if (xlength(xx3) == xlength(yy3)) {
              return -1;
            } else {

            }
          }
          break;
        case REALSXP:
          switch(TYPEOF(yy3)) {
          case INTSXP:
            return -3;
          default:
            return -1;
          }

        }
      }
    case INTSXP:
      switch(TYPEOF(xx3)) {
      case LGLSXP:
      case INTSXP:
      case REALSXP:
        return -5;
      }
    case REALSXP:
      switch(TYPEOF(xx3)) {
      case LGLSXP:
      case INTSXP:
      case REALSXP:
        return -5;
      }
    }
  case INTSXP:
    return Sum3I__(o1, INTEGER(xx1),
                   xlength(xx1),
                   yy1,
                   o2, xx2, yy2,
                   o3, xx3, yy3,
                   And, nThread);
  }
  return -1;
}




