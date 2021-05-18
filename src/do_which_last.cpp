#include "cpphutils.h"


// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_last_xi_ini(IntegerVector x, IntegerVector y) {
  R_xlen_t Nx = x.length();
  R_xlen_t Ny = y.length();
  if (Ny == 0) {
    return 0; // # nocov
  }

  if (Ny > 100) {
    // Above 100 elements it starts to become faster to use a hash map.
    // 100 fairly approximate but simply a round number
    std::unordered_set<int> H;
    for (R_xlen_t t = 0; t < Ny; ++t) {
      H.insert(y[t]);
    }

    for (R_xlen_t i = Nx - 1; i >= 0; --i) {
      int xi = x[i];
      if (H.count(xi)) {
        return i + 1;
      }
    }
  } else {
    for (R_xlen_t i = Nx - 1; i >= 0; --i) {
      int xi = x[i];
      for (R_xlen_t j = 0; j < Ny; ++j) {
        if (xi == y[j]) {
          return i + 1;
        }
      }
    }
  }
  return 0;
}


// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_last_xi_ind(IntegerVector x, DoubleVector yd) {
  R_xlen_t Nx = x.length();
  if (yd.length() == 0) {
    return 0; // # nocov
  }

  IntegerVector y = dblTable2int(yd);
  R_xlen_t Ny = y.length();

  if (Ny > 100) {
    // Above 100 elements it starts to become faster to use a hash map.
    // 100 fairly approximate but simply a round number
    std::unordered_set<int> H;
    for (R_xlen_t t = 0; t < Ny; ++t) {
      H.insert(y[t]);
    }

    for (R_xlen_t i = Nx - 1; i >= 0; --i) {
      int xi = x[i];
      if (H.count(xi)) {
        return i + 1;
      }
    }
  } else {
    for (R_xlen_t i = Nx - 1; i >= 0; --i) {
      int xi = x[i];
      for (R_xlen_t j = 0; j < Ny; ++j) {
        if (xi == y[j]) {
          return i + 1;
        }
      }
    }
  }
  return 0;
}

// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_last_xd_ind(DoubleVector x, DoubleVector y, bool y_has_na) {
  R_xlen_t Nx = x.length();
  R_xlen_t Ny = y.length();
  if (Ny == 0) {
    return 0; // # nocov
  }

  if (Ny > 100) {
    // Above 100 elements it starts to become faster to use a hash map.
    // 100 fairly approximate but simply a round number
    std::unordered_set<double> H;
    for (R_xlen_t t = 0; t < Ny; ++t) {
      H.insert(y[t]);
    }

    for (R_xlen_t i = Nx - 1; i >= 0; --i) {
      double xi = x[i];
      if (y_has_na && ISNAN(xi)) {
        return i + 1;
      }
      if (H.count(xi)) {
        return i + 1;
      }
    }
  } else {
    for (R_xlen_t i = Nx - 1; i >= 0; --i) {
      double xi = x[i];
      if (y_has_na && ISNAN(xi)) {
        return i + 1;
      }
      for (R_xlen_t j = 0; j < Ny; ++j) {
        if (xi == y[j]) {
          return i + 1;
        }
      }
    }
  }
  return 0;
}

