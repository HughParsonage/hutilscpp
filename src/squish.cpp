#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector squishn(NumericVector x, double a, double b) {
  int N = x.size();
  NumericVector out(N);
  for (int i = 0; i < N; ++i) {
    if (x[i] < a) {
      out[i] = a;
    } else if (x[i] > b) {
      out[i] = b;
    } else {
      out[i] = x[i];
    }
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector squishi(IntegerVector x, int a, int b) {
  int N = x.size();
  IntegerVector out(N);
  for (int i = 0; i < N; ++i) {
    if (x[i] < a) {
      out[i] = a;
    } else if (x[i] > b) {
      out[i] = b;
    } else {
      out[i] = x[i];
    }
  }
  return out;
}


