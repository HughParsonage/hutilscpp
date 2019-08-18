//' @title Parallel maximum in C++
//' @description A faster \code{pmax()}.
//'
//' @name do_pmaxV
//' @param x A numeric vector.
//' @param y A numeric vector, the same length as x.
//' @param in_place (bool, default: \code{false}) Should the function operate on \code{x} in-place?
//' @return The parallel maximum of the input values.
//' @export do_pmaxNumNum do_pmaxIntInt


#include <Rcpp.h>
using namespace Rcpp;

//' @rdname do_pmaxV
// [[Rcpp::export]]
NumericVector do_pmaxNumNum(NumericVector x, NumericVector y,
                            bool in_place = false) {
  const R_xlen_t n = x.length();
  if (n != y.length()){
    stop("x and y must be same length.");
  }
  R_xlen_t i = 0;
    while (i < n && x[i] > y[i]) {
      ++i;
    }

  if (i == n) {
    return x;
  }

  NumericVector out = in_place ? NumericVector(x) : NumericVector(clone(x));
  for (; i < n; ++i) {
    double xi = x[i];
    double yi = y[i];
    if (xi < yi) {
      out[i] = yi;
    }
  }
  return out;
}

//' @rdname do_pmaxV
// [[Rcpp::export]]
IntegerVector do_pmaxIntInt(IntegerVector x, IntegerVector y,
                            bool in_place = false) {
  const R_xlen_t n = x.length();
  if (n != y.length()){
    stop("x and y must be same length.");
  }
  R_xlen_t i = 0;
  while (i < n && x[i] >= y[i]) {
    ++i;
  }

  if (i == n) {
    return x;
  }

  IntegerVector out = in_place ? IntegerVector(x) : IntegerVector(clone(x));
  for (R_xlen_t i = 0; i < n; ++i) {
    int xi = x[i];
    int yi = y[i];
    if (xi < yi) {
      out[i] = yi;
    }
  }
  return out;
}
