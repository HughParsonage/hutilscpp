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


// [[Rcpp::export]]
R_xlen_t firstNonNegativeRadix(DoubleVector x,
                               R_xlen_t mini = 0,
                               R_xlen_t maxi = -1,
                               bool desc = false,
                               int depth = 0) {
  if (maxi < 0) {
    maxi = x.length();
  }
  if (mini < 0) {
    mini = 0;
  }
  if (desc) {
    if (x[mini] < 0 || x[maxi - 1] > 0) {
      return 0;
    }
  } else {
    if (x[mini] > 0 || x[maxi - 1] < 0) {
      return 0;
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
  return firstNonNegativeRadix(x, left, right, desc, depth + 1);
}

// [[Rcpp::export]]
DoubleVector do_pmax0_radix_sorted(DoubleVector x,
                                   bool in_place = false) {
  R_xlen_t n = x.size();
  bool desc = x[0] > 0;
  R_xlen_t root = firstNonNegativeRadix(x, 0, n, desc);

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
IntegerVector do_rev_int(IntegerVector x) {
  return Rcpp::rev(x);;
}

// [[Rcpp::export]]
DoubleVector do_rev_dbl(DoubleVector x, bool in_place = false) {
  if (!in_place) {
    return Rcpp::rev(x);
  }
  R_xlen_t n = x.length();
  DoubleVector out(x);
  R_xlen_t M = n / 2;
  for (R_xlen_t i = 0; i < M; ++i) {
    R_xlen_t j = n - i - 1;
    double xi = x[i];
    double xj = x[j];
    out[i] = xj;
    out[j] = xi;
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector do_sparse_int(R_xlen_t n, R_xlen_t pos, int val) {
  IntegerVector out = no_init(n);
  for (R_xlen_t i = 0; i < n; ++i) {
    out[i] = (i == pos) ? val : 0;
  }
  return out;
}

// [[Rcpp::export]]
DoubleVector do_sparse_dbl(R_xlen_t n, DoubleVector pos, DoubleVector val) {
  if (pos.length() != val.length()) {
    stop("lengths of pos and val differ."); // nocov
  }
  DoubleVector out = DoubleVector(n);
  for (R_xlen_t i = 0; i < pos.length(); ++i) {
    R_xlen_t j = pos[i] - 1;
    if (j < 0) continue;
    out[j] += val[i];
  }
  return out;
}





