#include "hutilscpp.h"

SEXP Cwhich_first(SEXP x) {
  if (TYPEOF(x) != LGLSXP) {
    error("Internal error(Cwhich_first): TYPEOF(x) != LGLSXP."); // # nocov
  }
  R_xlen_t N = xlength(x);
  R_xlen_t o = 0;
  const int * xp = LOGICAL(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] == TRUE) {
      o = i + 1;
      break;
    }
  }
  return ScalarLength(o);
}

SEXP Cwhich_last(SEXP x) {
  if (TYPEOF(x) != LGLSXP) {
    error("Internal error(Cwhich_first): TYPEOF(x) != LGLSXP."); // # nocov
  }
  R_xlen_t N = xlength(x);
  R_xlen_t o = 0;
  const int * xp = LOGICAL(x);
  for (R_xlen_t i = N - 1; i >= 0; --i) {
    if (xp[i] == TRUE) {
      o = i + 1;
      break;
    }
  }
  return ScalarLength(o);
}

SEXP Cwhich_first_false(SEXP x) {
  if (TYPEOF(x) != LGLSXP) {
    error("Internal error(Cwhich_first): TYPEOF(x) != LGLSXP."); // # nocov
  }
  R_xlen_t N = xlength(x);
  R_xlen_t o = 0;
  const int * xp = LOGICAL(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] == FALSE) {
      o = i + 1;
      break;
    }
  }
  return ScalarLength(o);
}

SEXP Cwhich_last_false(SEXP x) {
  if (TYPEOF(x) != LGLSXP) {
    error("Internal error(Cwhich_first): TYPEOF(x) != LGLSXP."); // # nocov
  }
  R_xlen_t N = xlength(x);
  R_xlen_t o = 0;
  const int * xp = LOGICAL(x);
  for (R_xlen_t i = N - 1; i >= 0; --i) {
    if (xp[i] == FALSE) {
      o = i + 1;
      break;
    }
  }
  return ScalarLength(o);
}

SEXP Cwhich_last_notTRUE(SEXP x) {
  if (TYPEOF(x) != LGLSXP) {
    error("Internal error(Cwhich_first): TYPEOF(x) != LGLSXP."); // # nocov
  }
  R_xlen_t N = xlength(x);
  R_xlen_t o = 0;
  const int * xp = LOGICAL(x);
  for (R_xlen_t i = N - 1; i >= 0; --i) {
    if (xp[i] != TRUE) {
      o = i + 1;
      break;
    }
  }
  return ScalarLength(o);
}

SEXP Cwhich_last_notFALSE(SEXP x) {
  if (TYPEOF(x) != LGLSXP) {
    error("Internal error(Cwhich_first): TYPEOF(x) != LGLSXP."); // # nocov
  }
  R_xlen_t N = xlength(x);
  R_xlen_t o = 0;
  const int * xp = LOGICAL(x);
  for (R_xlen_t i = N - 1; i >= 0; --i) {
    if (xp[i] != FALSE) {
      o = i + 1;
      break;
    }
  }
  return ScalarLength(o);
}

R_xlen_t which_first_in_lgl(SEXP xx, SEXP anyNAx, SEXP anyT, SEXP nAll) {
  const bool anyNA_ = asLogical(anyNAx);
  const bool any_ = asLogical(anyT);
  const bool nall_ = asLogical(nAll);

  R_xlen_t N = xlength(xx);
  if (TYPEOF(xx) != LGLSXP) {
    error("Internal error(which_first_in_lgl): TYPEOF(xx) != LGLSXP."); // # nocov
  }
  const int * x = LOGICAL(xx);

  // anyNA -> has NA
  // any_ -> has TRUE
  // nall -> not FALSE

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
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] == FALSE) {
        return i + 1;
      }
    }
  }

  // 2
  if (!anyNA_ & any_ & !nall_) {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] == TRUE) {
        return i + 1;
      }
    }
  }

  // 3
  if (!anyNA_ & any_ & nall_) {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] != NA_LOGICAL) {
        return i + 1;
      }
    }
  }

  // 4
  if (anyNA_ & !any_ & !nall_) {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] == NA_LOGICAL) {
        return i + 1;
      }
    }
  }

  // 5
  if (anyNA_ & !any_ & nall_) {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] != TRUE) {
        return i + 1;
      }
    }
  }

  // 6
  if (anyNA_ & any_ & !nall_) {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] != FALSE) {
        return i + 1;
      }
    }
  }

  if (anyNA_ & any_ & nall_) {
    return 1;
  }
  return 0;
}

SEXP Cwhich_first_in_lgl(SEXP xx, SEXP anyNAx, SEXP anyT, SEXP nAll) {
  return ScalarLength(which_first_in_lgl(xx, anyNAx, anyT, nAll));
}

SEXP Cwhich_firstNA(SEXP x) {
  R_xlen_t N = xlength(x);
  R_xlen_t o = 0;
  switch(TYPEOF(x)) {
  case NILSXP:
    return ScalarLength(0);
    break;

  case LGLSXP:
  case INTSXP: {
    const int * xp = TYPEOF(x) == LGLSXP ? LOGICAL(x) : INTEGER(x);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xp[i] == NA_INTEGER) {
        o = i + 1;
        break;
      }
    }
  }
    break;
  case REALSXP: {
    const double * xp = REAL(x);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (ISNAN(xp[i])) {
        o = i + 1;
        break;
      }
    }
  }
    break;
  case STRSXP: {
    for (R_xlen_t i = 0; i < N; ++i) {
    if (STRING_ELT(x, i) == NA_STRING) {
      o = i + 1;
      break;
    }
  }
  }
  }
  return ScalarLength(o);
}

SEXP Cwhich_lastNA(SEXP x) {
  R_xlen_t N = xlength(x);
  R_xlen_t o = 0;
  switch(TYPEOF(x)) {
  case NILSXP:
    return ScalarLength(0);
    break;

  case LGLSXP:
  case INTSXP: {
    const int * xp = TYPEOF(x) == LGLSXP ? LOGICAL(x) : INTEGER(x);
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (xp[i] == NA_INTEGER) {
        o = i + 1;
        break;
      }
    }
  }
    break;
  case REALSXP: {
    const double * xp = REAL(x);
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (ISNAN(xp[i])) {
        o = i + 1;
        break;
      }
    }
  }
    break;
  case STRSXP: {
    for (R_xlen_t i = N - 1; i >= 0; --i) {
    if (STRING_ELT(x, i) == NA_STRING) {
      o = i + 1;
      break;
    }
  }
  }
  }
  return ScalarLength(o);
}

R_xlen_t do_which_first_lgl_lgl_op(SEXP xx, SEXP yy, int op, bool reverse) {
  if (op == OP_BO) {
    // even (F, T) can never occur
    return 0;
  }
  if (TYPEOF(xx) != LGLSXP || TYPEOF(yy) != LGLSXP) {
    return -1; // # nocov
  }

  R_xlen_t N = xlength(xx), Ny = xlength(yy);
  if (N == 0 || Ny == 0 || (N != Ny && op != OP_BW)) {
    return 0; // # nocov
  }

  const int * x = LOGICAL(xx);
  const int * y = LOGICAL(yy);

  if (op == OP_BW) {
    int y0 = y[0] == NA_LOGICAL ? 0 : y[0];
    int y1 = y[1] == NA_LOGICAL ? 1 : y[1];

    if (y0 > y1) {
      return 0;
    }
    if (y0 < y1) {
      return reverse ? N : 1;
    }
    if (y0) {
      // only TRUE
      for (R_xlen_t k = 0; k < N; ++k) {
        R_xlen_t i = reverse ? (N - k - 1) : k;
        if (x[i] == 1) {
          return i + 1;
        }
      }
    } else {
      // only FALSE
      for (R_xlen_t k = 0; k < N; ++k) {
        R_xlen_t i = reverse ? (N - k - 1) : k;
        if (x[i] == 0) {
          return i + 1;
        }
      }
    }
    return 0;
  }

  for (R_xlen_t k = 0; k < N; ++k) {
    R_xlen_t i = reverse ? (N - k - 1) : k;
    int xi = x[i];
    int y1 = y[i];
    if (isingle_ox_x1_x2(xi, op, y1, y1)) {
      return i + 1;
    }
  }
  return 0;
}

SEXP Cwhich_first_lgl_lgl_op(SEXP xx, SEXP yy, SEXP opp, SEXP reverse) {
  const int op = asInteger(opp);
  const bool rev = asLogical(reverse);
  R_xlen_t N = xlength(xx);
  R_xlen_t Ny = xlength(yy);
  const bool len_eq = Ny == N;
  const bool len1 = Ny == 1;
  if (!len_eq && !len1 && op != OP_IN && op != OP_BW && op != OP_BO) {
    error("Lengths differ."); // # nocov
  }
  if (op == OP_IN || op == OP_BW) {
    if (op == OP_BW && Ny != 2) {
      error("%between% expects RHS to be a vector of length-2."); // # nocov
    }
  }
  if (TYPEOF(xx) != LGLSXP || TYPEOF(yy) != LGLSXP) {
    error("Internal error:(Cwhich_first_lgl_lgl_op): TYPEOF(x) != LGLSXP && TYPEOF(y) != LGLSXP"); // # nocov
  }
  return ScalarLength(do_which_first_lgl_lgl_op(xx, yy, op, rev));
}

R_xlen_t do_which_first_xd_ad(const double * x,
                              int op,
                              double a,
                              R_xlen_t N) {
  switch(op) {
  case OP_NE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] != a) {
        return i + 1;
      }
    }
    break;
  case OP_EQ:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] == a) {
        return i + 1;
      }
    }
    break;
  case OP_GE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] >= a) {
        return i + 1;
      }
    }
    break;
  case OP_GT:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] > a) {
        return i + 1;
      }
    }
    break;
  case OP_LE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] <= a) {
        return i + 1;
      }
    }
    break;
  case OP_LT:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] < a) {
        return i + 1;
      }
    }
    break;
  }
  return 0;
}

R_xlen_t do_which_first_xd_yd(const double * x,
                              int op,
                              const double * y,
                              R_xlen_t N) {
  for (R_xlen_t i = 0; i < N; ++i) {
    if (dsingle_ox_x1_x2(x[i], op, y[i], 0)) {
      return i + 1;
    }
  }

  return 0;
}

R_xlen_t do_which_first_xd_yi(const double * x,
                              int op,
                              const int * y,
                              R_xlen_t N) {
  switch(op) {
  case OP_NE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] != y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_EQ:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] == y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_GE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] >= y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_GT:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] > y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_LE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] <= y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_LT:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] < y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  }

  return 0;
}

R_xlen_t do_which_first_xi_yi(const int * x, int op, const int * y, R_xlen_t N) {

  switch(op) {
  case OP_NE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] != y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_EQ:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] == y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_GE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] >= y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_GT:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] > y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_LE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] <= y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_LT:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] < y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  }

  return 0;
}

R_xlen_t do_which_first_xi_yd(const int * x, int op, const double * y, R_xlen_t N) {

  switch(op) {
  case OP_NE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] == NA_INTEGER || ISNAN(y[i])) continue;
      if (x[i] != y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_EQ:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] == y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_GE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] >= y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_GT:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] > y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_LE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] <= y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_LT:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] < y[i]) {
        return i + 1;
      }
    }
    break; // # nocov
  }

  return 0;
}

R_xlen_t do_which_first_xi_aii(const int * x, int op, int a1, int a2, R_xlen_t N) {

  // a1 = NA_INTEGER is already INT MIN so no need to treat
  if (a2 == NA_INTEGER) {
    a2 = INT_MAX;
  }
  if (a1 > a2) {
    return 0;
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    if (isingle_ox_x1_x2(x[i], op, a1, a2)) {
      return i + 1;
    }
  }
  return 0;
}

R_xlen_t do_which_first_xd_add(const double * x,
                               int op,
                               double a1,
                               double a2,
                               R_xlen_t N) {

  if (ISNAN(a1)) {
    a1 = R_NegInf;
  }
  if (ISNAN(a2)) {
    a2 = R_PosInf;
  }
  if (a1 > a2) {
    return 0;
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    if (dsingle_ox_x1_x2(x[i], op, a1, a2)) {
      return i + 1;
    }
  }
  return 0;
}

R_xlen_t do_which_first_xi_ai(const int * x,
                              int op,
                              int a,
                              R_xlen_t N) {
  switch(op) {
  case OP_NE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] != a) {
        return i + 1;
      }
    }
    break;
  case OP_EQ:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] == a) {
        return i + 1;
      }
    }
    break;
  case OP_GE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] >= a) {
        return i + 1;
      }
    }
    break;
  case OP_GT:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] > a) {
        return i + 1;
      }
    }
    break;
  case OP_LE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] <= a) {
        return i + 1;
      }
    }
    break;
  case OP_LT:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] < a) {
        return i + 1;
      }
    }
    break;
  }
  return 0;
}

R_xlen_t do_which_first_xi_ad(const int * x,
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
      return 1;
    }
    if (!R_finite(ad)) {
      if (ad == R_PosInf) {
        switch(op) {
        case OP_GE:
          return 0;
        case OP_LE:
          return 1;
        case OP_GT:
          return 0;
        case OP_LT:
          return 1;
        }
      }
      if (ad == R_NegInf) {
        switch(op) {
        case OP_GE:
          return 1;
        case OP_LE:
          return 0;
        case OP_GT:
          return 1;
        case OP_LT:
          return 0;
        }
      }
      return 0; // # nocov
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
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] != a) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_EQ:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] == a) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_GE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] >= a) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_GT:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] > a) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_LE:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] <= a) {
        return i + 1;
      }
    }
    break; // # nocov
  case OP_LT:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] < a) {
        return i + 1;
      }
    }
    break; // # nocov
  }
  return 0;
}

R_xlen_t do_which_first_xi_add(const int * x, int op, double a1, double a2, R_xlen_t N) {

  if (ISNAN(a1) || a1 < -2147483647) {
    a1 = R_NegInf;
  }
  if (ISNAN(a2) || a1 > 2147483647) {
    a2 = R_PosInf;
  }
  if (a1 > a2) {
    return 0;
  }
  switch(op) {
  case OP_BW:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] >= a1 && x[i] <= a2) {
        return i + 1;
      }
    }
    break;
  case OP_BO:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] > a1 && x[i] < a2) {
        return i + 1;
      }
    }
    break;
  case OP_BC:
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] <= a1 || x[i] >= a2) {
        return i + 1;
      }
    }
    break;
  }
  return 0;
}

R_xlen_t which_first__(SEXP xx, SEXP opp, SEXP yy,
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
  R_xlen_t Ny = xlength(yy);
  if (ny > 2 && Nx != Ny) {
    error("Internal error(which_first__): ny > 2 && Nx != Ny."); // # nocov
  }

  if (TYPEOF(xx) == INTSXP && TYPEOF(yy) == INTSXP) {

    const int * x = INTEGER(xx);
    const int * y = INTEGER(yy);
    switch(ny) {
    case 1:
      return do_which_first_xi_ai(x, op, y1i, Nx);
    case 2:
      return do_which_first_xi_aii(x, op, y1i, y2i, Nx);
    default:
      return do_which_first_xi_yi(x, op, y, Nx);
    }
  }
  if (TYPEOF(xx) == INTSXP && TYPEOF(yy) == REALSXP) {
    const int * x = INTEGER(xx);
    const double * y = REAL(yy);
    switch(ny) {
    case 1:
      return do_which_first_xi_ad(x, op, y1d, Nx);
    case 2:
      return do_which_first_xi_add(x, op, y1d, y2d, Nx);
    default:
      return do_which_first_xi_yd(x, op, y, Nx);
    }
  }
  if (TYPEOF(xx) == REALSXP && TYPEOF(yy) == INTSXP) {
    const double * x = REAL(xx);
    const int * y = INTEGER(yy);
    switch(ny) {
    case 1:
      return do_which_first_xd_ad(x, op, y1d, Nx);
    case 2:
      return do_which_first_xd_add(x, op, y1d, y2d, Nx);
    default:
      return do_which_first_xd_yi(x, op, y, Nx);
    }
  }
  if (TYPEOF(xx) == REALSXP && TYPEOF(yy) == REALSXP) {
    const double * x = REAL(xx);
    const double * y = REAL(yy);
    switch(ny) {
    case 1:
      return do_which_first_xd_ad(x, op, y1d, Nx);
    case 2:
      return do_which_first_xd_add(x, op, y1d, y2d, Nx);
    default:
      return do_which_first_xd_yd(x, op, y, Nx);
    }
  }
  return 0; // # nocov
}

SEXP Cwhich_first__(SEXP xx, SEXP opp, SEXP yy,
                    SEXP nyy,
                    SEXP y1ii,
                    SEXP y2ii,
                    SEXP y1dd,
                    SEXP y2dd) {
  return ScalarLength(which_first__(xx, opp, yy,
                                    nyy,
                                    y1ii,
                                    y2ii,
                                    y1dd,
                                    y2dd));
}



SEXP C_which_first_lgl1(SEXP X, SEXP A, SEXP O, SEXP R) {
  const int * xp = LOGICAL(X);
  const int a = asLogical(A);
  const bool r = asLogical(R);
  const int o = asInteger(O);
  R_xlen_t N = xlength(X);
  if (r) {
    for (R_xlen_t i = N - 1; i >= 0; --i) {
      if (isingle_ox_x1_x2(xp[i], o, a, a)) {
        return ScalarLength(i + 1);
      }
    }
  } else {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (isingle_ox_x1_x2(xp[i], o, a, a)) {
        return ScalarLength(i + 1);
      }
    }
  }
  return ScalarInteger(0);
}



