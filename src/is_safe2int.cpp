#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
int is_safe2int(DoubleVector x, double int_max) {
  double int_min = -int_max;
  R_xlen_t n = x.length();
  int out = 1;
  for (R_xlen_t i = 0; i < n; ++i) {
    double xi = x[i];
    // (int)NaN is UBD

    if (R_finite(xi) && xi <= int_max && xi >= int_min) {
      int xint = (int)xi;
      if (xint != xi) {
        return 0;
      }
      continue;
    }
    if (R_IsNA(xi) || R_IsNaN(xi)) {
      out = 2;
      continue;
    }
    if (!R_finite(xi)) {
      return 0;
    }
    if (xi > int_max) {
      return 0;
    } else if (xi + int_max <= 0) {
      return 0;
    }
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector force_as_integer(DoubleVector x, int na_code) {
  R_xlen_t n = x.size();
  IntegerVector out = no_init(n);
  // (int) non-finites are UBD
  switch(na_code) {
  case 0:
    stop("Internal error: na_code = 0 so cannot return IntegerVector safely.");
  case 1:
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] = (int)x[i];
    }
  case 2:
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] = (R_finite(x[i])) ? (int)x[i] : NA_INTEGER;
    }
  }
  return out;
}

