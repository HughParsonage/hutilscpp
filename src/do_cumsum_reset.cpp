#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector do_cumsum_reset_logical(LogicalVector x) {
  R_xlen_t N = x.size();
  IntegerVector out(N);
  // first element does not require loop
  if (x[0]) {
    out[0] = 1;
  } else {
    out[0] = 0;
  }
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i]) {
      if (x[i - 1]) {
        out[i] = out[i - 1] + 1;
      } else {
        out[i] = 1;
      }
    } else {
      // reset
      out[i] = 0;
    }
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector do_cumsum_reset_integer(LogicalVector x, IntegerVector y) {
  R_xlen_t N = x.size();
  IntegerVector out(N);
  if (x[0]) {
    out[0] = y[0];
  } else {
    out[0] = 0;
  }
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i]) {
      if (x[i - 1]) {
        out[i] = out[i - 1] + y[i];
      } else {
        out[i] = y[i];
      }
    } else {
      // reset
      out[i] = 0;
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector do_cumsum_reset_double(LogicalVector x, NumericVector y) {
  R_xlen_t N = x.size();
  NumericVector out(N);
  if (x[0]) {
    out[0] = y[0];
  } else {
    out[0] = 0;
  }
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i]) {
      if (x[i - 1]) {
        out[i] = out[i - 1] + y[i];
      } else {
        out[i] = y[i];
      }
    } else {
      // reset
      out[i] = 0;
    }
  }
  return out;
}

// [[Rcpp::export]]
LogicalVector do_duplicated_sorted_int(IntegerVector x) {
  R_xlen_t n = x.length();
  LogicalVector out = no_init(n);
  out[0] = false;
  for (R_xlen_t i = 1; i < n; ++i) {
    out[i] = x[i] == x[i - 1];
  }
  return out;
}

// [[Rcpp::export]]
LogicalVector do_duplicated_sorted_dbl(DoubleVector x) {
  R_xlen_t n = x.length();
  LogicalVector out = no_init(n);
  out[0] = false;
  for (R_xlen_t i = 1; i < n; ++i) {
    out[i] = x[i] == x[i - 1];
  }
  return out;
}


// [[Rcpp::export]]
IntegerVector do_cumsum_reset_sorted_int(IntegerVector x) {
  R_xlen_t n = x.length();
  IntegerVector out = no_init(n);
  out[0] = 1;
  for (R_xlen_t i = 1; i < n; ++i) {
    out[i] = (x[i] == x[i - 1]) ? (out[i - 1] + 1) : 1;
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector do_N_by(IntegerVector x) {
  R_xlen_t n = x.length();

  if (n == 1) {
    IntegerVector out1 = IntegerVector(1);
    out1[0] = 1;
    return out1;
  }
  if (n == 2) {
    IntegerVector out2 = IntegerVector(2);
    if (x[0] == x[1]) {
      out2[0] = 2;
      out2[1] = 2;
    } else {
      out2[0] = 1;
      out2[1] = 1;
    }
    return out2;
  }


  IntegerVector out = no_init(n);
  out[0] = 1;
  for (R_xlen_t i = 1; i < n; ++i) {
    out[i] = (x[i] == x[i - 1]) ? (out[i - 1] + 1) : 1;
  }
  IntegerVector outN = IntegerVector(n);
  outN[n - 1] = out[n - 1];
  for (R_xlen_t i = n - 2; i >= 1; --i) {
    int thisN = out[i];
    // int prevN = outN[i - 1];
    int nextN = outN[i + 1];

    outN[i] = (x[i] == x[i + 1]) ? nextN : thisN;
  }
  if (x[1] == x[0]) {
    outN[0] = outN[1];
  } else {
    outN[0] = 1;
  }

  return outN;
}




