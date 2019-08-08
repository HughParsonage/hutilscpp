#include <Rcpp.h>
#include "cpphutils.h"
using namespace Rcpp;


// [[Rcpp::export]]
DoubleVector do_pmax0_abs_dbl(DoubleVector x,
                              bool in_place = false) {
  R_xlen_t n = x.size();
  R_xlen_t j = 0;
  while (j < n && x[j] >= 0) {
    ++j;
  }
  if (j == n) {
    return x;
  }

  DoubleVector out(in_place ? x : clone(x));
  for (R_xlen_t i = 0; i < n; ++i) {
    if (i < j) {
      out[i] = x[i];
      continue;
    }
    out[i] += std::fabs(out[i]);
    out[i] /= 2;
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector do_pmax0_abs_int(IntegerVector x,
                               bool in_place = false) {
  R_xlen_t n = x.size();
  R_xlen_t j = 0;

  // First, is x already nonnegative?  We assume it is
  // but verify.  Either we reach 'n' (in which case
  // just return x or we just start from there when
  // allocating result.
  while (j < n && x[j] >= 0) {
    ++j;
  }
  if (j >= n) {
    return x;
  }

  // At this point, x is known to be negative, so we
  // have to allocate a new result:
  IntegerVector out = no_init(n);
  for (R_xlen_t i = 0; i < n; ++i) {
    if (i < j) {
      out[i] = x[i];
      continue;
    }
    int64_t oi = x[i];
    oi += std::abs(oi);
    out[i] = static_cast<int>(oi / 2);
  }
  return out;
}

void showValuex(const char* what, double x) {
  Rcout << " " << what << " \t " << x << std::endl;
  // return 0;
}

// [[Rcpp::export]]
R_xlen_t do_firstNonNegativeRadix_int(IntegerVector x,
                                      R_xlen_t mini = 0,
                                      R_xlen_t maxi = -1,
                                      bool desc = false,
                                      int depth = 0) {

  R_xlen_t xsize = x.length();
  if (maxi < 0 || maxi > xsize) {
    maxi = xsize;
  }
  if (mini < 0) {
    mini = 0;
  }
  int lastx = x[maxi - 1];

  if (desc) {
    if (x[mini] < 0 || lastx > 0) {
      return mini;
    }
  } else {
    if (x[mini] > 0 || lastx < 0) {
      return mini;
    }
  }

  if (mini > maxi - 8) {
    for (R_xlen_t i = mini; i < maxi; ++i) {
      if (desc) {
        if (x[i] <= 0) {
          return i;
        }
      } else {
        if (x[i] >= 0) {
          return i;
        }
      }
    }
    return maxi;
  }
  R_xlen_t medi = mini + (maxi - mini) / 2;
  bool lhs = (x[medi] < 0) ? desc : !desc;
  R_xlen_t left = lhs ? mini : medi - 1;
  R_xlen_t right = lhs ? medi + 1 : maxi;
  return do_firstNonNegativeRadix_int(x, left, right, desc, depth + 1);
}


// [[Rcpp::export]]
R_xlen_t do_firstNonNegativeRadix_dbl(DoubleVector x,
                                      R_xlen_t mini = 0,
                                      R_xlen_t maxi = -1,
                                      bool desc = false,
                                      int depth = 0) {

  R_xlen_t xsize = x.length();
  if (maxi < 0 || maxi > xsize) {
    maxi = xsize;
  }
  if (mini < 0) {
    mini = 0;
  }
  double lastx = x[maxi - 1];


  if (desc) {
    if (x[mini] < 0 || lastx > 0) {
      return mini;
    }
  } else {
    if (x[mini] > 0 || lastx < 0) {
      return mini;
    }
  }

  if (mini > maxi - 8) {
    // showValuex("depth = ", depth);
    for (R_xlen_t i = mini; i < maxi; ++i) {
      if (desc) {
        if (x[i] <= 0) {
          return i;
        }
      } else {
        if (x[i] >= 0) {
          return i;
        }
      }
    }
    return maxi;
  }
  R_xlen_t medi = mini + (maxi - mini) / 2;
  bool lhs = (x[medi] < 0) ? desc : !desc;
  R_xlen_t left = lhs ? mini : medi - 1;
  R_xlen_t right = lhs ? medi + 1 : maxi;
  return do_firstNonNegativeRadix_dbl(x, left, right, desc, depth + 1);
}

// [[Rcpp::export]]
DoubleVector do_pmax0_radix_sorted_dbl(DoubleVector x,
                                       bool in_place = false) {
  R_xlen_t n = x.size();
  bool x0_positive = x[0] > 0;
  bool xn_positive = x[n - 1] > 0;
  if (x0_positive && xn_positive) {
    return x;
  }
  bool desc = x[0] > 0;
  R_xlen_t root = do_firstNonNegativeRadix_dbl(x, 0, n, desc);

  DoubleVector out(in_place ? x : clone(x));
  if (desc) {
    for (R_xlen_t i = root; i < n; ++i) {
      out[i] = 0;
    }
  } else {
    for (R_xlen_t i = 0; i < root; ++i) {
      out[i] = 0;
    }
  }

  return out;
}

// [[Rcpp::export]]
IntegerVector do_pmax0_radix_sorted_int(IntegerVector x,
                                       bool in_place = false) {
  R_xlen_t n = x.size();
  bool x0_positive = x[0] > 0;
  bool xn_positive = x[n - 1] > 0;
  if (x0_positive && xn_positive) {
    return x;
  }
  bool desc = x0_positive;
  R_xlen_t root = do_firstNonNegativeRadix_int(x, 0, n, desc);

  IntegerVector out(in_place ? x : clone(x));
  if (desc) {
    for (R_xlen_t i = root; i < n; ++i) {
      out[i] = 0;
    }
  } else {
    for (R_xlen_t i = 0; i < root; ++i) {
      out[i] = 0;
    }
  }

  return out;
}





