#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int anyOutside_int(IntegerVector x, int a, int b, bool nas_present = false, bool na_is_outside = true) {
  int N = x.size();
  if (nas_present) {
    if (na_is_outside) {
      for (int i = 0; i < N; ++i) {
        if (x[i] == R_NaInt || x[i] < a || x[i] > b) {
          return ++i;
        }
      }
    } else {
      for (int i = 0; i < N; ++i) {
        if (x[i] == R_NaInt) {
          continue;
        }
        if (x[i] < a || x[i] > b) {
          return ++i;
        }
      }
    }
  } else {
    for (int i = 0; i < N; ++i) {
      if (x[i] < a || x[i] > b) {
        return ++i;
      }
    }
  }
  return 0;
}

// [[Rcpp::export]]
int anyOutside_dbl(DoubleVector x, double a, double b, bool nas_present = false, bool na_is_outside = true) {
  int N = x.size();
  if (nas_present) {
    if (na_is_outside) {
      for (int i = 0; i < N; ++i) {
        if (R_IsNA(x[i]) || x[i] < a || x[i] > b) {
          return ++i;
        }
      }
    } else {
      for (int i = 0; i < N; ++i) {
        if (R_IsNA(x[i])) {
          continue;
        }
        if (x[i] < a || x[i] > b) {
          return ++i;
        }
      }
    }
  } else {
    // No NAs to check
    for (int i = 0; i < N; ++i) {
      if (x[i] < a || x[i] > b) {
        return ++i;
      }
    }
  }
  return 0;
}
