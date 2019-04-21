#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector do_or3(LogicalVector x, LogicalVector y, LogicalVector z) {
  int N = x.size();
  if (y.length() != N) {
    stop("y and x have different lengths.");
  }
  LogicalVector out(N);
  if (z.length() != N) {
    if (z.length() > 1) {
      stop("z has the wrong length");
    }
    if (z.length() == 0) {
      for (int i = 0; i < N; ++i) {
        out[i] = x[i] || y[i];
      }
    } else {
     if (z[0]) {
       for (int i = 0; i < N; ++i) {
         out[i] = true;
       }
     } else {
       for (int i = 0; i < N; ++i) {
         out[i] = x[i] || y[i];
       }
     }

    }
  } else {
    for (int i = 0; i < N; ++i) {
      out[i] = x[i] || y[i] || z[i];
    }
  }
  return out;
}

// [[Rcpp::export]]
LogicalVector do_and3(LogicalVector x, LogicalVector y, LogicalVector z) {
  int N = x.size();
  if (y.length() != N) {
    stop("y and x have different lengths.");
  }
  LogicalVector out(N);
  if (z.length() != N) {
    if (z.length() > 1) {
      stop("z has the wrong length");
    }
    // if NULL -> fall back to binary &
    // if TRUE -> equivalent to binary &
    if (z.length() == 0 || z[0]) {
      for (int i = 0; i < N; ++i) {
        out[i] = x[i] && y[i];
      }
    } else {
      // z = false so all are false
      return out;


    }
  } else {
    for (int i = 0; i < N; ++i) {
      out[i] = x[i] && y[i] && z[i];
    }
  }
  return out;
}

// [[Rcpp::export]]
LogicalVector na_and (LogicalVector x) {
  // NA & x
  int n = x.length();
  LogicalVector out(n);
  for (int i = 0; i < n; ++i) {
    if (x[i] != FALSE) {
      out[i] = NA_LOGICAL;
    }
  }
  return out;
}


