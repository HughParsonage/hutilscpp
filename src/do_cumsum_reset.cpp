#include "cpphutils.h"

#if (__GNUC__ > 7) || \
((__GNUC__ == 7) && (__GNUC_MINOR__ > 3))
#define BUILTIN_MUL_OVERFLOW_EXIST
#endif

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

// [[Rcpp::export(rng = false)]]
IntegerVector do_cumsum_reset_where(IntegerVector x,
                                    IntegerVector y,
                                    int o,
                                    int a) {
  R_xlen_t N = x.length();
  if (N != y.length() || N == 0) {
    stop("Internal error(do_cumsum_reset_where): length(x) != length(y)");
  }
  IntegerVector out = no_init(N);
  out[0] = single_ox_x1_x2(y[0], o, a, a) ? 0 : x[0];
  for (R_xlen_t i = 1; i < N; ++i) {
    if (single_ox_x1_x2(y[i], o, a, a) || x[i] == NA_INTEGER) {
      out[i] = out[i - 1];
      continue;
    }

    // avoid overflow here
    int64_t w = (int64_t)out[i - 1];
    w += x[i];
    if (w >= INT_MAX || w <= INT_MIN) {
      out[i] = NA_INTEGER;
      continue;
    }
    out[i] = (int)w;
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
IntegerVector add_(IntegerVector x, IntegerVector y) {
  R_xlen_t N = x.length();
  IntegerVector out = no_init(N);
  for (R_xlen_t i = 0; i < N; ++i) {

#ifdef BUILTIN_MUL_OVERFLOW_EXIST
    int p = 0;
    bool did_overflow = __builtin_sadd_overflow(x[i], y[i], &p);
#else
    int64_t p = x[i];
    p += y[i];
    bool did_overflow =
      p >= INT_MAX || p <= -INT_MAX || x[i] == NA_INTEGER || y[i] == NA_INTEGER;
#endif
    if (did_overflow) {
      out[i] = NA_INTEGER;
    } else {
      out[i] = p;
    }
  }
  return out;
}













