#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector do_are_even(IntegerVector x, DoubleVector y, int wb = 0) {
  int N = x.size();
  int M = y.size();
  const bool is_int = N > 0;
  LogicalVector out = no_init(is_int ? N : M);
  int wc = (wb > 1) ? (wb - 1) : N;

  if (is_int) {
    for (int i = 0; i < N; ++i) {
      if (x[i] == NA_INTEGER) {
        out[i] = NA_LOGICAL;
        continue;
      }
      out[i] = !(x[i] % 2);
    }
  } else {
    for (int i = 0; i < M; ++i) {
      if (i >= wc && !R_finite(y[i])) {
        out[i] = NA_LOGICAL;
        continue;
      }
      int yi = y[i];
      out[i] = !(yi % 2);
    }
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector do_which_even(IntegerVector x, DoubleVector y, int wb = 0) {
  int N = x.size();
  int M = y.size();
  if (M >= INT_MAX || N >= INT_MAX) {
    stop("Internal error: long vectors are not supported."); // # nocov
  }
  int wc = (wb > 1) ? (wb - 1) : N;
  const bool is_int = N > 0;
  std::vector<int> out1(0);
  if (is_int) {
    for (int i = 0; i < N; ++i) {
      // NA integer is even
      if ((x[i] % 2) == 0) {
        int j = i + 1;  // indexing
        out1.push_back(j);
      }
    }
  } else {
    for (int i = 0; i < M; ++i) {
      double yi0 = y[i];
      if (i >= wc && !R_finite(yi0)) {
        continue;
      }
      int yi = yi0;
      if ((yi % 2) == 0) {
        int j = i + 1;  // indexing
        out1.push_back(j);
      }
    }
  }
  return wrap(out1);
}
