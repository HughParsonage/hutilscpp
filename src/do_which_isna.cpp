#include "cpphutils.h"

// [[Rcpp::export(rng = false)]]
IntegerVector do_which_isna_int(IntegerVector x, bool isnt = false) {
  R_xlen_t N = x.length();
  std::vector<int> o;
  o.reserve(N);
  if (isnt) {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] != NA_INTEGER) {
        o.push_back(i + 1);
      }
    }
  } else {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (x[i] == NA_INTEGER) {
        o.push_back(i + 1);
      }
    }
  }
  return wrap(o);
}
