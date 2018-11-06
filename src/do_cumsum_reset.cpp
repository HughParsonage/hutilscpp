#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector do_cumsum_reset_logical(LogicalVector x) {
  int N = x.size();
  IntegerVector out(N);
  // first element does not require loop
  if (x[0]) {
    out[0] = 1;
  } else {
    out[0] = 0;
  }
  for (int i = 1; i < N; ++i) {
    if (x[i]) {
      if (x[i - 1]) {
        out[i] = out[i - 1] + 1;
      } else {
        out[i] = 1;
      }
    } else {
      // reset
      out[i] = 0;
    }
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector do_cumsum_reset_integer(LogicalVector x, IntegerVector y) {
  int N = x.size();
  IntegerVector out(N);
  if (x[0]) {
    out[0] = y[0];
  } else {
    out[0] = 0;
  }
  for (int i = 1; i < N; ++i) {
    if (x[i]) {
      if (x[i - 1]) {
        out[i] = out[i - 1] + y[i];
      } else {
        out[i] = y[i];
      }
    } else {
      // reset
      out[i] = 0;
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector do_cumsum_reset_double(LogicalVector x, NumericVector y) {
  int N = x.size();
  NumericVector out(N);
  if (x[0]) {
    out[0] = y[0];
  } else {
    out[0] = 0;
  }
  for (int i = 1; i < N; ++i) {
    if (x[i]) {
      if (x[i - 1]) {
        out[i] = out[i - 1] + y[i];
      } else {
        out[i] = y[i];
      }
    } else {
      // reset
      out[i] = 0;
    }
  }
  return out;
}

