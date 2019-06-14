#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
R_xlen_t sum_isna_int(IntegerVector x) {
  R_xlen_t n = x.size();
  R_xlen_t out = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    if (x[i] == NA_INTEGER) {
      out += 1;
    }
  }
  return out;
}



