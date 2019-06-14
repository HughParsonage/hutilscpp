//' @title Parallel maximum
//' @description A faster \code{pmin()}.
//'
//' @name do_pminV
//' @param x A numeric vector.
//' @param y A numeric vector, the same length as x.
//' @param in_place (bool, default: \code{false}) Modify \code{x} in-place?
//' @return The parallel maximum of the input values.
//' @export do_pminV_dbl do_pminV_int

#include <Rcpp.h>
using namespace Rcpp;

//' @rdname do_pminV
// [[Rcpp::export]]
NumericVector do_pminV_dbl(NumericVector x, NumericVector y, bool in_place = false) {
  const R_xlen_t n = x.length();
  if (n != y.length()){
    stop("x and y must be same length.");
  }
  NumericVector out = in_place ? NumericVector(x) : NumericVector(clone(x));
  for (R_xlen_t i = 0; i < n; ++i) {
    double xi = x[i];
    double yi = y[i];
    if (yi < xi) {
      out[i] = yi;
    }
  }
  return out;
}

//' @rdname do_pminV
// [[Rcpp::export]]
IntegerVector do_pminV_int(IntegerVector x, IntegerVector y, bool in_place = false) {
  const R_xlen_t n = x.length();
  if (n != y.length()){
    stop("x and y must be same length.");
  }
  IntegerVector out = in_place ? IntegerVector(x) : IntegerVector(clone(x));
  for (R_xlen_t i = 0; i < n; ++i) {
    int xi = x[i];
    int yi = y[i];
    if (yi < xi) {
      out[i] = yi;
    }
  }
  return out;
}
