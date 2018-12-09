#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int do_which_first(LogicalVector x) {
  int N = x.size();
  for (int i = 0; i < N; ++i) {
    if (x[i]) {
      return ++i;
    }
  }

  return 0;
}

// [[Rcpp::export]]
int do_which_last(LogicalVector x) {
  int N = x.size();
  for (int i = N - 1; i >= 0; --i) {
    if (x[i]) {
      return ++i;
    }
  }

  return 0;
}

// [[Rcpp::export]]
int do_which_first_false (LogicalVector x) {
  int N = x.size();
  for (int i = 0; i < N; ++i) {
    if (!x[i]) {
      return ++i;
    }
  }
  return 0;
}


