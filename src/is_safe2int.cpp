#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
int is_safe2int(DoubleVector x, double int_max) {
  R_xlen_t n = x.length();
  for (R_xlen_t i = 0; i < n; ++i) {
    double xi = x[i];
    if (R_IsNA(xi)) {
      continue;
    }
    if (xi > int_max) {
      return 0;
    } else if (xi + int_max <= 0) {
      return 0;
    }
    int xint = (int)xi;
    if (xint != xi) {
      return 0;
    }
  }
  return 1;
}

// [[Rcpp::export]]
IntegerVector force_as_integer(DoubleVector x) {
  R_xlen_t n = x.size();
  IntegerVector out(n);
  for (R_xlen_t i = 0; i < n; ++i) {
    out[i] = (int)x[i];
  }
  return out;
}

