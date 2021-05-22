#include "hutilscpp.h"


R_xlen_t do_which_last_xd_ad(const double * x,
                             int op,
                             double a,
                             R_xlen_t N) {
  switch(op) {
  case OP_NE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] != a) {
        return i + 1;
      }
    }
    break;
  case OP_EQ:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] == a) {
        return i + 1;
      }
    }
    break;
  case OP_GE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] >= a) {
        return i + 1;
      }
    }
    break;
  case OP_GT:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] > a) {
        return i + 1;
      }
    }
    break;
  case OP_LE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] <= a) {
        return i + 1;
      }
    }
    break;
  case OP_LT:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] < a) {
        return i + 1;
      }
    }
    break;
  }
  return 0;
}


R_xlen_t do_which_last_xi_ai(const int * x,
                             int op,
                             int a,
                             R_xlen_t N) {
  switch(op) {
  case OP_NE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] != a) {
        return i + 1;
      }
    }
    break;
  case OP_EQ:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] == a) {
        return i + 1;
      }
    }
    break;
  case OP_GE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] >= a) {
        return i + 1;
      }
    }
    break;
  case OP_GT:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] > a) {
        return i + 1;
      }
    }
    break;
  case OP_LE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] <= a) {
        return i + 1;
      }
    }
    break;
  case OP_LT:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] < a) {
        return i + 1;
      }
    }
    break;
  }
  return 0;
}


R_xlen_t do_which_last_xi_ad(const int * x,
                             int op,
                             double ad,
                             R_xlen_t N) {
  if (N == 0) {
    return 0; // # nocov
  }
  int e = 0;
  if (!do_is_safe2int(ad)) {
    if (op == OP_EQ || op == OP_IN || ISNAN(ad)) {
      return 0;
    }
    if (op == OP_NE) {
      return N;
    }
    if (!R_finite(ad)) {
      if (ad == R_PosInf) {
        switch(op) {
        case OP_GE:
          return 0;
        case OP_LE:
          return N;
        case OP_GT:
          return 0;
        case OP_LT:
          return N;
        }
      }
      if (ad == R_NegInf) {
        switch(op) {
        case OP_GE:
          return N;
        case OP_LE:
          return 0;
        case OP_GT:
          return N;
        case OP_LT:
          return 0;
        }
      }
    }
    // correct for integer conversion giving
    // incorrect results for >  >=  <  <=
    if (op == OP_GT) {
      // x >  2.5 => x > 2
      // x > -2.5 => x > -3
      e -= (ad < 0);
    }
    if (op == OP_GE) {
      // 3,4,5   >=  4.5  => 3,4,5 >= 5
      e += (ad > 0);
    }
    if (op == OP_LT) {
      // x <  2.5 => x < 3
      // x < -2.5 => x < -2
      e += (ad > 0);
    }
    if (op == OP_LE) {
      // x <= 4.5  => x <= 4
      // x <= -4.5 => x <= -4
      e -= (ad < 0);
    }
  }
  double ade = ((ad < 0) ? ceil(ad) : floor(ad)) + e;
  if (ade >= INT_MAX) {
    ade = INT_MAX;
  }
  if (ade <= -INT_MAX) {
    ade = -INT_MAX;
  }
  const int a = (int)ade;

  switch(op) {
  case OP_NE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] != a) {
        return i + 1;
      }
    }
    break;
  case OP_EQ:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] == a) {
        return i + 1;
      }
    }
    break;
  case OP_GE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] >= a) {
        return i + 1;
      }
    }
    break;
  case OP_GT:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] > a) {
        return i + 1;
      }
    }
    break;
  case OP_LE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] <= a) {
        return i + 1;
      }
    }
    break;
  case OP_LT:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] < a) {
        return i + 1;
      }
    }
    break;
  }
  return 0;
}




R_xlen_t do_which_last_xd_yd(const double * x, int op, const double * y, R_xlen_t N) {
  switch(op) {
  case OP_NE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] != y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_EQ:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] == y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_GE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] >= y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_GT:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] > y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_LE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] <= y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_LT:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] < y[i]) {
        return i + 1;
      }
    }
    break;
  }

  return 0;
}


R_xlen_t do_which_last_xi_yi(const int * x, int op, const int * y, R_xlen_t N) {
  switch(op) {
  case OP_NE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] != y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_EQ:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] == y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_GE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] >= y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_GT:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] > y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_LE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] <= y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_LT:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] < y[i]) {
        return i + 1;
      }
    }
    break;
  }

  return 0;
}


R_xlen_t do_which_last_xi_yd(const int * x, int op, const double * y, R_xlen_t N) {
  switch(op) {
  case OP_NE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] == NA_INTEGER || ISNAN(y[i])) continue;
      if (x[i] != y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_EQ:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] == y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_GE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] >= y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_GT:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] > y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_LE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] <= y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_LT:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] < y[i]) {
        return i + 1;
      }
    }
    break;
  }

  return 0;
}


R_xlen_t do_which_last_xd_yi(const double * x, int op, const int * y, R_xlen_t N) {
  switch(op) {
  case OP_NE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] != y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_EQ:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] == y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_GE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] >= y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_GT:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] > y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_LE:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] <= y[i]) {
        return i + 1;
      }
    }
    break;
  case OP_LT:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] < y[i]) {
        return i + 1;
      }
    }
    break;
  }

  return 0;
}



R_xlen_t do_which_last_xi_aii(const int * x, int op, int a1, int a2, R_xlen_t N) {
  if (a2 == NA_INTEGER) {
    a2 = INT_MAX;
  }
  switch(op) {
  case OP_BW:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] >= a1 && x[i] <= a2) {
        return i + 1;
      }
    }
    break;
  case OP_BO:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] > a1 && x[i] < a2) {
        return i + 1;
      }
    }
    break;
  case OP_BC:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] <= a1 || x[i] >= a2) {
        return i + 1;
      }
    }
    break;
  }
  return 0;
}

R_xlen_t do_which_last_xi_add(const int * x, int op, double a1, double a2,
                              R_xlen_t N) {
  if (ISNAN(a1) || a1 < -2147483647) {
    a1 = R_NegInf;
  }
  if (ISNAN(a2) || a1 > 2147483647) {
    a2 = R_PosInf;
  }

  switch(op) {
  case OP_BW:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] >= a1 && x[i] <= a2) {
        return i + 1;
      }
    }
    break;
  case OP_BO:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] > a1 && x[i] < a2) {
        return i + 1;
      }
    }
    break;
  case OP_BC:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] <= a1 || x[i] >= a2) {
        return i + 1;
      }
    }
    break;
  }
  return 0;
}


R_xlen_t do_which_last_xd_add(const double * x, int op, double a1, double a2, R_xlen_t N) {
  if (ISNAN(a1)) {
    a1 = R_NegInf;
  }
  if (ISNAN(a2)) {
    a2 = R_PosInf;
  }
  switch(op) {
  case OP_BW:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] >= a1 && x[i] <= a2) {
        return i + 1;
      }
    }
    break;
  case OP_BO:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] > a1 && x[i] < a2) {
        return i + 1;
      }
    }
    break;
  case OP_BC:
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] <= a1 || x[i] >= a2) {
        return i + 1;
      }
    }
    break;
  }
  return 0;
}


R_xlen_t which_last__(SEXP xx, SEXP opp, SEXP yy,
                      SEXP nyy,
                      SEXP y1ii,
                      SEXP y2ii,
                      SEXP y1dd,
                      SEXP y2dd) {
  const int op = asInteger(opp);
  const int ny = asInteger(nyy);
  const int y1i = asInteger(y1ii);
  const int y2i = asInteger(y2ii);
  const double y1d = asReal(y1dd);
  const double y2d = asReal(y2dd);
  R_xlen_t Nx = xlength(xx);

  if (TYPEOF(xx) == INTSXP && TYPEOF(yy) == INTSXP) {
    const int * x = INTEGER(xx);
    const int * y = INTEGER(yy);
    switch(ny) {
    case 1:
      return do_which_last_xi_ai(x, op, y1i, Nx);
    case 2:
      return do_which_last_xi_aii(x, op, y1i, y2i, Nx);
    default:
      return do_which_last_xi_yi(x, op, y, Nx);
    }
  }
  if (TYPEOF(xx) == INTSXP && TYPEOF(yy) == REALSXP) {
    const int * x = INTEGER(xx);
    const double * y = REAL(yy);
    switch(ny) {
    case 1:
      return do_which_last_xi_ad(x, op, y1d, Nx);
    case 2:
      return do_which_last_xi_add(x, op, y1d, y2d, Nx);
    default:
      return do_which_last_xi_yd(x, op, y, Nx);
    }
  }
  if (TYPEOF(xx) == REALSXP && TYPEOF(yy) == INTSXP) {
    const double * x = REAL(xx);
    const int * y = INTEGER(yy);
    switch(ny) {
    case 1:
      return do_which_last_xd_ad(x, op, y1d, Nx);
    case 2:
      return do_which_last_xd_add(x, op, y1d, y2d, Nx);
    default:
      return do_which_last_xd_yi(x, op, y, Nx);
    }
  }
  if (TYPEOF(xx) == REALSXP && TYPEOF(yy) == REALSXP) {
    const double * x = REAL(xx);
    const double * y = REAL(yy);
    switch(ny) {
    case 1:
      return do_which_last_xd_ad(x, op, y1d, Nx);
    case 2:
      return do_which_last_xd_add(x, op, y1d, y2d, Nx);
    default:
      return do_which_last_xd_yd(x, op, y, Nx);
    }
  }
  return 0; // # nocov
}




R_xlen_t do_which_last_in_lgl(const int * x, bool anyNA_, bool any_, bool nall_, R_xlen_t N) {
  // anyNA -> has NA
  // any_ -> has TRUE
  // nall -> has FALSE

  // anyNA  any_   nall_
  //     0     0       0  / 0
  //     0     0       1  / 1
  //     0     1       0  / 2
  //     0     1       1  / 3
  //     1     0       0  / 4
  //     1     0       1  / 5
  //     1     1       0  / 6
  //     1     1       1  / 7


  // 0
  if (!anyNA_ & !any_ & !nall_) {
    return 0;
  }

  // 1
  if (!anyNA_ & !any_ &  nall_) {
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] == FALSE) {
        return i + 1;
      }
    }
  }

  // 2
  if (!anyNA_ & any_ & !nall_) {
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] == TRUE) {
        return i + 1;
      }
    }
  }

  // 3
  if (!anyNA_ & any_ & nall_) {
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] != NA_LOGICAL) {
        return i + 1;
      }
    }
  }

  // 4
  if (anyNA_ & !any_ & !nall_) {
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] == NA_LOGICAL) {
        return i + 1;
      }
    }
  }

  // 5
  if (anyNA_ & !any_ & nall_) {
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] != TRUE) {
        return i + 1;
      }
    }
  }

  // 6
  if (anyNA_ & any_ & !nall_) {
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (x[i] != FALSE) {
        return i + 1;
      }
    }
  }

  if (anyNA_ & any_ & nall_) {
    return N;
  }
  return 0;
}

SEXP Cwhich_last_in_lgl(SEXP xx, SEXP anyNA_, SEXP any_, SEXP nall_) {
  if (TYPEOF(xx) != LGLSXP) {
    error("Internal error(Cwhich_last_in_lgl): TYPEOF(xx) != LGLSXP."); // # nocov
  }
  R_xlen_t N = xlength(xx);
  if (N == 0) {
    error("Internal error(Cwhich_last_in_lgl): N == 0."); // # nocov
  }
  const int * x = LOGICAL(xx);
  return ScalarLength(do_which_last_in_lgl(x, asLogical(anyNA_), asLogical(any_), asLogical(nall_), N));
}

SEXP Cwhich_last__(SEXP xx, SEXP opp, SEXP yy,
                    SEXP nyy,
                    SEXP y1ii,
                    SEXP y2ii,
                    SEXP y1dd,
                    SEXP y2dd) {
  int ny = asInteger(nyy);
  R_xlen_t Nx = xlength(xx);
  R_xlen_t Ny = xlength(yy);
  if (ny > 2 && Nx != Ny) {
    error("Internal error(which_last__): ny > 2 && Nx != Ny."); // # nocov
  }
  if (Nx == 0 || Ny == 0) {
    error("Internal error(which_last__): Nx == 0 || Ny == 0."); // # nocov
  }
  return ScalarLength(which_last__(xx, opp, yy,
                                   nyy,
                                   y1ii,
                                   y2ii,
                                   y1dd,
                                   y2dd));
}


