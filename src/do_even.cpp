#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector do_are_even (IntegerVector x, DoubleVector y) {
  R_xlen_t N = x.size();
  int M = y.size();
  const bool is_int = N > 0;
  LogicalVector out(is_int ? N : M);
  if (is_int) {
    for (R_xlen_t i = 0; i < N; ++i) {
      if ((x[i] % 2) == 0) {
        out[i] = true;
      }
    }
  } else {
    for (R_xlen_t i = 0; i < M; ++i) {
      int yi = y[i];
      if ((yi % 2) == 0) {
        out[i] = true;
      }
    }
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector do_which_even (IntegerVector x, DoubleVector y) {
  int N = x.size();
  int M = y.size();
  const bool is_int = N > 0;
  std::vector<int> out1(0);
  if (is_int) {
    for (int i = 0; i < N; ++i) {
      if ((x[i] % 2) == 0) {
        int j = i + 1;  // indexing
        out1.push_back(j);
      }
    }
  } else {
    for (int i = 0; i < M; ++i) {
      double yi0 = y[i];
      if (R_finite(yi0)) {
        int yi = yi0;
        if ((yi % 2) == 0) {
          int j = i + 1;  // indexing
          out1.push_back(j);
        }
      }
    }
  }
  return wrap(out1);
}
