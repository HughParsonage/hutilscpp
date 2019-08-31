/*
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double do_sum_dbl_par(NumericVector x) {
  double o = 0;
  int n = x.length();
#pragma omp parallel for reduction(+:o)
  for (int i = 0; i < n; ++i) {
    o = o + x[i];
  }
  return o;
}

 */


