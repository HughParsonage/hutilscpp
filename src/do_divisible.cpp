#include "cpphutils.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector do_divisible(IntegerVector x, int d, int nThread = 1) {
  R_xlen_t N = x.length();
  LogicalVector out = no_init(N);

#pragma omp parallel for num_threads(nThread)
  for (R_xlen_t i = 0; i < N; ++i) {
    out[i] = !(x[i] % d);
  }
  return out;
}

// [[Rcpp::export]]
LogicalVector do_divisible16(IntegerVector x, int nThread = 1) {
  R_xlen_t N = x.length();
  LogicalVector out = no_init(N);

#pragma omp parallel for num_threads(nThread)
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xi = (unsigned int)x[i];
    out[i] = !(xi % 16);
  }
  return out;
}
