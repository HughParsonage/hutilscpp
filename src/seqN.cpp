#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(rng = false)]]
IntegerVector seqN(IntegerVector x, IntegerVector y, int m = 1) {
  int N = x.length();
  if (N != y.length()) stop("n not ylen");
  IntegerVector out = no_init(N);
  if (m == 0) {
    for (int i = 0; i < N; ++i) {
      out[i] = x[i] + y[i];
    }
  } else if (m == 1) {
    for (int i = 0; i < N; ++i) {
      out[i] = -1;
      if (i > 0 && x[i - 1] > x[i] && x[i] < 10) {
        continue;
      }
      if (y[i] < 14 && (x[i] % 2)) {
        continue;
      }
      out[i] = x[i] + y[i];
    }
  } else {
    for (int i = 0; i < N; ++i) {
      out[i] = -1;
      if (x[i] < 10 && i > 0 && x[i - 1] > x[i]) {
        continue;
      }
      out[i] = x[i] + y[i];
    }
  }
  return out;
}
