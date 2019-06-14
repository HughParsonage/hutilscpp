#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int do_which_first(LogicalVector x) {
  R_xlen_t N = x.size();
  for (R_xlen_t i = 0; i < N; ++i) {
    if (x[i]) {
      return ++i;
    }
  }

  return 0;
}

// [[Rcpp::export]]
int do_which_last(LogicalVector x) {
  R_xlen_t N = x.size();
  for (R_xlen_t i = N - 1; i >= 0; --i) {
    if (x[i]) {
      return ++i;
    }
  }

  return 0;
}

// [[Rcpp::export]]
int do_which_first_false (LogicalVector x) {
  R_xlen_t N = x.size();
  for (R_xlen_t i = 0; i < N; ++i) {
    if (!x[i]) {
      return ++i;
    }
  }
  return 0;
}

// [[Rcpp::export]]
int do_which_first_int_int (IntegerVector x,
                            IntegerVector y,
                            bool eq = true,
                            bool gt = false,
                            bool lt = false) {
  const R_xlen_t n = x.size();
  const R_xlen_t m = y.size();
  if (n != m) {
    stop("lengths x and y differ.");
  }
  // != == >= <=  >  <
  //  0  1  2  3  4  5
  const int op = !(eq || gt || lt) ? 0 : (eq ? (gt ? 2 : (lt ? 3 : 1)) : (gt ? 4 : 5));

  // if (op == 0) {
  //   for (R_xlen_t i = 0; i < n; ++i) {
  //     int xi = x[i];
  //     int yi = y[i];
  //     if (xi != yi) {
  //       return ++i;
  //     }
  //   }
  //   return 0;
  // }

  for (R_xlen_t i = 0; i < n; ++i) {
    switch (op) {
    case 0:
      if (x[i] != y[i]) {
        return ++i;
      }
      continue;
    case 1:
      if (x[i] == y[i]) {
        return ++i;
      }
      continue;
    case 2:
      if (x[i] >= y[i]) {
        return ++i;
      }
      continue;
    case 3:
      if (x[i] <= y[i]) {
        return ++i;
      }
      continue;
    case 4:
      if (x[i] > y[i]) {
        return ++i;
      }
      continue;
    case 5:
      if (x[i] < y[i]) {
        return ++i;
      }
      continue;

    }
  }
  return 0;

}


