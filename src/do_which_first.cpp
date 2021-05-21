#include "cpphutils.h"






// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_first_xi_ini(IntegerVector x, IntegerVector y) {
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

    for (R_xlen_t i = 0; i < Nx; ++i) {
      int xi = x[i];
      if (H.count(xi)) {
        return i + 1;
      }
    }
  } else {
    for (R_xlen_t i = 0; i < Nx; ++i) {
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

int cpp_type_safe2int(double x) {
  if (ISNAN(x)) {
    return 2;
  }
  if (x < -2147483647 || x > 2147483647) {
    return 0;
  }
  int xi = (int)x;
  return (xi == x) ? 1 : 0;
}

// [[Rcpp::export(rng = false)]]
IntegerVector dblTable2int(DoubleVector table) {
  R_xlen_t N = table.length();
  std::set<int> o;
  // o.reserve(N);

  for (R_xlen_t i = 0; i < N; ++i) {
    double xd = table[i];
    switch(cpp_type_safe2int(xd)) {
    case 2:
      o.insert(NA_INTEGER);
      break;
    case 1:
      o.insert(static_cast<int>(xd));
      break;
    }
  }
  return wrap(o);
}


// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_first_xi_ind(IntegerVector x, DoubleVector yd) {
  R_xlen_t Nx = x.length();
  if (yd.length() == 0) {
    return 0;
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

    for (R_xlen_t i = 0; i < Nx; ++i) {
      int xi = x[i];
      if (H.count(xi)) {
        return i + 1;
      }
    }
  } else {
    for (R_xlen_t i = 0; i < Nx; ++i) {
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
R_xlen_t do_which_first_xd_ind(DoubleVector x, DoubleVector y, bool y_has_na) {
  R_xlen_t Nx = x.length();
  R_xlen_t Ny = y.length();
  if (Ny == 0) {
    return 0;
  }



  if (Ny > 100) {
    // Above 100 elements it starts to become faster to use a hash map.
    // 100 fairly approximate but simply a round number
    std::unordered_set<double> H;
    for (R_xlen_t t = 0; t < Ny; ++t) {
      H.insert(y[t]);
    }

    for (R_xlen_t i = 0; i < Nx; ++i) {
      double xi = x[i];
      if (y_has_na && ISNAN(xi)) {
        return i + 1;
      }
      if (H.count(xi)) {
        return i + 1;
      }
    }
  } else {
    for (R_xlen_t i = 0; i < Nx; ++i) {
      double xi = x[i];
      for (R_xlen_t j = 0; j < Ny; ++j) {
        if (y_has_na && ISNAN(xi)) {
          return i + 1;
        }
        if (xi == y[j]) {
          return i + 1;
        }
      }
    }
  }
  return 0;
}
