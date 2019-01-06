#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector squishn(NumericVector x, double a, double b) {
  int N = x.size();
  NumericVector out(x);
  for (int i = 0; i < N; ++i) {
    if (x[i] < a) {
      out[i] = a;
    } else if (x[i] > b) {
      out[i] = b;
    }
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector squishi(IntegerVector x, int a, int b) {
  int N = x.size();
  IntegerVector out(x);
  for (int i = 0; i < N; ++i) {
    if (x[i] < a) {
      out[i] = a;
    } else if (x[i] > b) {
      out[i] = b;
    }
  }
  return out;
}


