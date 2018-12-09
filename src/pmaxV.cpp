//' @title Parallel maximum in C++
//' @description A faster \code{pmax()}.
//'
//' @name pmaxV
//' @param x A numeric vector.
//' @param y A numeric vector, the same length as x.
//' @return The parallel maximum of the input values.
//' @export do_pmaxNumNum do_pmaxIntInt


#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector do_pmaxNumNum(NumericVector x, NumericVector y) {
  int n = x.length();
  int m = y.length();
  if (n != m){
    stop("x and y must be same length.");
  }
  NumericVector out(n);

  for (int i = 0; i < n; ++i) {
    double xi = x[i];
    double yi = y[i];
    if (xi < yi) {
      out[i] = yi;
    } else {
      out[i] = xi;
    }
  }

  return out;
}

// [[Rcpp::export]]
IntegerVector do_pmaxIntInt(IntegerVector x, IntegerVector y) {
  int n = x.length();
  int m = y.length();
  if (n != m){
    stop("x and y must be same length.");
  }
  IntegerVector out(n);
  for (int i = 0; i < n; ++i) {
    int xi = x[i];
    int yi = y[i];
    if (xi < yi) {
      out[i] = yi;
    } else {
      out[i] = xi;
    }
  }

  return out;
}
