#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DoubleVector do_summary3_dbl(DoubleVector x, DoubleVector y, DoubleVector z,
                             bool in_place = false,
                             bool do_max = true) {
  const int n = x.length();
  const int ny = y.length();
  const int nz = z.length();
  if ((n != ny) || (n != nz)){
    stop("x, y, z must have the same length.");
  }
  NumericVector out = in_place ? NumericVector(x) : NumericVector(clone(x));

  if (do_max) {
    for (int i = 0; i < n; ++i) {
      double xi = x[i];
      double yi = y[i];
      double zi = z[i];
      if (xi < zi && yi < zi){
        out[i] = zi;
      } else {
        if (xi < yi){
          out[i] = yi;
        }
      }
    }
  } else {
    for (int i = 0; i < n; ++i) {
      double xi = x[i];
      double yi = y[i];
      double zi = z[i];
      if (xi > zi && yi > zi){
        out[i] = zi;
      } else {
        if (xi > yi){
          out[i] = yi;
        }
      }
    }
  }
  return out;
}


// [[Rcpp::export]]
IntegerVector do_summary3_int(IntegerVector x, IntegerVector y, IntegerVector z,
                       bool in_place = false,
                       bool do_max = true) {
  const int n = x.length();
  const int ny = y.length();
  const int nz = z.length();
  if ((n != ny) || (n != nz)){
    stop("x, y, z must have the same length.");
  }
  IntegerVector out = in_place ? IntegerVector(x) : IntegerVector(clone(x));

  if (do_max) {
    for (int i = 0; i < n; ++i) {
      int xi = x[i];
      int yi = y[i];
      int zi = z[i];
      if (xi < zi && yi < zi){
        out[i] = zi;
      } else {
        if (xi < yi){
          out[i] = yi;
        }
      }
    }
  } else {
    for (int i = 0; i < n; ++i) {
      int xi = x[i];
      int yi = y[i];
      int zi = z[i];
      if (xi > zi && yi > zi){
        out[i] = zi;
      } else {
        if (xi > yi){
          out[i] = yi;
        }
      }
    }
  }
  return out;
}
