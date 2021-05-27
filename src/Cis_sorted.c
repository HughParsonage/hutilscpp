#include "hutilscpp.h"

bool is_sorted_ascending_dbl(const double * x, R_xlen_t N) {
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i] < x[i - 1]) {
      return false;
    }
  }
  return true;
}

R_xlen_t do_isntSorted_ascending_dbl(const double * x, R_xlen_t N) {
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i] < x[i - 1]) {
      return i + 1;
    }
  }
  return 0;
}


bool is_sorted_descending_dbl(const double * x, R_xlen_t N) {
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i] > x[i - 1]) {
      return false;
    }
  }
  return true;
}

R_xlen_t do_isntSorted_descending_dbl(const double * x, R_xlen_t N) {
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i - 1] < x[i]) {
      return i + 1;
    }
  }
  return 0;
}


bool is_sorted_ascending_int(const int * x, R_xlen_t N) {
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i] < x[i - 1]) {
      return false;
    }
  }
  return true;
}

R_xlen_t do_isntSorted_ascending_int(const int * x, R_xlen_t N) {
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i] < x[i - 1]) {
      return i + 1;
    }
  }
  return 0;
}


bool is_sorted_descending_int(const int * x, R_xlen_t N) {
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i] > x[i - 1]) {
      return false;
    }
  }
  return true;
}

R_xlen_t do_isntSorted_descending_int(const int * x, R_xlen_t N) {
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i] > x[i - 1]) {
      return i + 1;
    }
  }
  return 0;
}


bool is_sorted_int(const int * x, R_xlen_t N) {
  if (N <= 2) {
    return true;
  }
  int xhead = x[0];
  int xtail = x[N - 1];

  if (xhead == xtail) {
    R_xlen_t i = 1;
    while (i < N && x[i] == xhead) {
      ++i;
    }
    return (i == N);
  }
  if (xhead < xtail) {
    return is_sorted_ascending_int(x, N);
  } else {
    return is_sorted_descending_int(x, N);
  }
}


R_xlen_t do_isntSorted_int(const int * x, R_xlen_t N) {
  if (N <= 2) {
    return 0;
  }
  int xhead = x[0];
  int xtail = x[N - 1];

  if (xhead == xtail) {
    R_xlen_t i = 1;
    while (i < N && x[i] == xhead) {
      ++i;
    }
    if (i == N) {
      return 0;
    }
    if (x[i] > xhead) {
      while (i < N && x[i] >= x[i - 1]) {
        ++i;
      }
      return i + 1;
    } else {
      while (i < N && x[i] <= x[i - 1]) {
        ++i;
      }
      return i + 1;
    }
  }
  if (xhead < xtail) {
    return do_isntSorted_ascending_int(x, N);
  } else {
    return do_isntSorted_descending_int(x, N);
  }
}


bool is_sorted_dbl(const double * x, R_xlen_t N) {
  if (N <= 2) {
    return true;
  }
  double xhead = x[0];
  double xtail = x[N - 1];

  if (xhead == xtail) {
    R_xlen_t i = 1;
    while (i < N && x[i] == xhead) {
      ++i;
    }
    return (i == N);
  }
  if (xhead < xtail) {
    return is_sorted_ascending_dbl(x, N);
  } else {
    return is_sorted_descending_dbl(x, N);
  }
}


R_xlen_t do_isntSorted_dbl(const double * x, R_xlen_t N) {
  if (N <= 2) {
    return 0;
  }
  double xhead = x[0];
  double xtail = x[N - 1];

  if (xhead == xtail) {
    R_xlen_t i = 1;
    while (i < N && x[i] == xhead) {
      ++i;
    }
    if (i == N) {
      return 0;
    }
    if (x[i] > xhead) {
      while (i < N && x[i] >= x[i - 1]) {
        ++i;
      }
      return i + 1;
    } else {
      while (i < N && x[i] <= x[i - 1]) {
        ++i;
      }
      return i + 1;
    }
  }
  if (xhead < xtail) {
    return do_isntSorted_ascending_dbl(x, N);
  } else {
    return do_isntSorted_descending_dbl(x, N);
  }
}

SEXP Cis_sorted(SEXP x, SEXP asc) {
  R_xlen_t N = xlength(x);
  if (N <= 1) {
    return ScalarLogical(1);
  }
  int ascending = asInteger(asc);
  switch(ascending) {
  case NA_INT:
    switch(TYPEOF(x)) {
    case INTSXP:
      return ScalarLogical(is_sorted_int(INTEGER(x), N));
    case REALSXP:
      return ScalarLogical(is_sorted_dbl(REAL(x), N));
    }
  case 0:
    switch(TYPEOF(x)) {
    case INTSXP:
      return ScalarLogical(is_sorted_descending_int(INTEGER(x), N));
    case REALSXP:
      return ScalarLogical(is_sorted_descending_dbl(REAL(x), N));
    }
  case 1:
    switch(TYPEOF(x)) {
    case INTSXP:
      return ScalarLogical(is_sorted_ascending_int(INTEGER(x), N));
    case REALSXP:
      return ScalarLogical(is_sorted_ascending_dbl(REAL(x), N));
    }
  }
  return R_NilValue; // # nocov
}

SEXP Cisnt_sorted(SEXP x, SEXP asc) {
  R_xlen_t N = xlength(x);
  if (N <= 1) {
    return ScalarInteger(0);
  }
  int ascending = asInteger(asc);
  switch(ascending) {
  case NA_INT:
    switch(TYPEOF(x)) {
    case INTSXP:
      return ScalarLength(do_isntSorted_int(INTEGER(x), N));
    case REALSXP:
      return ScalarLength(do_isntSorted_dbl(REAL(x), N));
    }
  case 0:
    switch(TYPEOF(x)) {
    case INTSXP:
      return ScalarLength(do_isntSorted_descending_int(INTEGER(x), N));
    case REALSXP:
      return ScalarLength(do_isntSorted_descending_dbl(REAL(x), N));
    }
  case 1:
    switch(TYPEOF(x)) {
    case INTSXP:
      return ScalarLength(do_isntSorted_ascending_int(INTEGER(x), N));
    case REALSXP:
      return ScalarLength(do_isntSorted_ascending_dbl(REAL(x), N));
    }
  }
  return R_NilValue; // # nocov
}
