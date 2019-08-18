#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
R_xlen_t sum_isna_int(IntegerVector x) {
  R_xlen_t n = x.size();
  R_xlen_t out = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    if (x[i] == NA_INTEGER) {
      out += 1;
    }
  }
  return out;
}

// [[Rcpp::export]]
R_xlen_t sum_isna_dbl(DoubleVector x) {
  R_xlen_t n = x.size();
  R_xlen_t out = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    if (R_IsNA(x[i])) {
      out += 1;
    }
  }
  return out;
}

// [[Rcpp::export]]
R_xlen_t sum_isna_complx(ComplexVector x) {
  R_xlen_t n = x.size();
  R_xlen_t out = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    Rcomplex v = COMPLEX_ELT(x, i);
    if (ISNAN(v.r) || ISNAN(v.i)) {
      out += 1;
    }
  }
  return out;
}

// [[Rcpp::export]]
R_xlen_t sum_isna_char(CharacterVector x) {
  R_xlen_t n = x.size();
  R_xlen_t out = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    if (x[i] == NA_STRING) {
      out += 1;
    }
  }
  return out;
}

// [[Rcpp::export]]
R_xlen_t sum_isfalse(LogicalVector x) {
  R_xlen_t n = x.size();
  R_xlen_t out = n;
  for (R_xlen_t i = 0; i < n; ++i) {
    bool xi = x[i];
    out -= xi;
  }
  return out;
}

// [[Rcpp::export]]
R_xlen_t sum_isna_logi(LogicalVector x) {
  R_xlen_t n = x.size();
  R_xlen_t out = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    if (x[i] == NA_LOGICAL) {
      out += 1;
    }
  }
  return out;
}


