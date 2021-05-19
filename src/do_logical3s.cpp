#include "cpphutils.h"

inline int opn(bool eq, bool gt, bool lt) {
  // != == >= <=  >  <
  //  0  1  2  3  4  5
  return !(eq || gt || lt) ? 0 : (eq ? (gt ? 2 : (lt ? 3 : 1)) : (gt ? 4 : 5));
}




// [[Rcpp::export(rng = false)]]
IntegerVector do_which_in(IntegerVector x, IntegerVector tbl) {
  R_xlen_t n = x.size();
  if (n >= INT_MAX) {
    stop("Length exceeds integer max.");  // # nocov
  }

  int m = (int)n;
  int tbln = tbl.size();
  std::vector<int> o;
  o.reserve(m);
  for (int i = 0; i < m; ++i) {
    int xi = x[i];
    if (xi == NA_INTEGER) {
      continue;
    }
    for (int j = 0; j < tbln; ++j) {
      if (x[i] == tbl[j]) {
        o.push_back(i + 1);
        break;
      }
    }
  }
  return wrap(o);
}

// # nocov start
std::vector<int> do_intersect3_stdint(std::vector<int> x,
                                      std::vector<int> y,
                                      std::vector<int> z) {
  int nx = x.size();
  int ny = y.size();
  int nz = z.size();

  std::vector<int> o;
  o.reserve(nx);


  int j = 0;
  int k = 0;
  for (int i = 0; i < nx; ++i) {
    int xi = x[i];
    while (j < ny && y[j] < xi) {
      ++j;
    }
    if (j >= ny) {
      break; // not possible to have intersections
    }
    int yj = y[j];
    if (xi > yj) {
      continue;
    }
    while (k < nz && z[k] < xi) {
      ++k;
    }
    if (k >= nz) {
      break;
    }
    int zk = z[k];
    if (xi == yj && xi == zk) {
      o.push_back(xi);
    }
  }
  return o;
}
// # nocov end

// [[Rcpp::export(rng = false)]]
bool test_intersect3_stdint(int M = 0) {
  std::vector<int> a = {1, 2, 3};
  std::vector<int> b = {1, 2, 3};
  std::vector<int> c = {1, 2, 3};
  if (M == 4) {
    a.push_back(M);
  }
  if (M == 5) {
    a.push_back(5);
    a.push_back(7);
    b.push_back(4);
    c.push_back(6);
  }

  std::vector<int> d = do_intersect3_stdint(a, b, c);
  // # nocov start
  for (int i = 0; i < 3; ++i) {
    if (d[i] != a[i]) {
      return false;
    }
  }
  // # nocov end
  return true;
}



bool single_ox_x1_x2(int x, int oix, int x1, int x2) {
  switch(oix) {
  case OP_NE:
    return x != x1;

  case OP_EQ:
    return x == x1;

  case OP_GE:
    return x >= x1;

  case OP_LE:
    return x <= x1;

  case OP_GT:
    return x >  x1;

  case OP_LT:
    return x <  x1;

  case OP_BW:
    return x >= x1 && x <= x2;

  case OP_BO:
    return x >  x1 && x <  x2;

  case OP_BC:
    return x <= x1 || x >= x2;
  }
  return false; // # nocov
}

bool single_ox_x1_x2(double x, int oix, double x1, double x2) {
  switch(oix) {
  case OP_NE:
    return x != x1;

  case OP_EQ:
    return x == x1;

  case OP_GE:
    return x >= x1;

  case OP_LE:
    return x <= x1;

  case OP_GT:
    return x >  x1;

  case OP_LT:
    return x <  x1;

  case OP_BW:
    return x >= x1 && x <= x2;

  case OP_BO:
    return x >  x1 && x <  x2;

  case OP_BC:
    return x <= x1 || x >= x2;
  }
  return false; // # nocov
}


// [[Rcpp::export(rng = false)]]
LogicalVector do_par_in(IntegerVector x, IntegerVector table, int nThread = 1) {
  R_xlen_t n = x.length();
  int tn = table.length();
  std::vector<int> tango;
  tango.reserve(tn);
  for (int ti = 0; ti < tn; ++ti) {
    tango.push_back(table[ti]);
  }
  sort(tango.begin(), tango.end());
  int a = tango[0];
  int b = tango[tn - 1];

  LogicalVector out = no_init(n);

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) shared(out, tango)
#endif
  for (R_xlen_t i = 0; i < n; ++i) {
    bool oi = false;
    int xi = x[i];
    if (xi >= a && xi <= b) {
      for (int j = 0; j < tn; ++j) {
        int tj = tango[j];
        if (x[i] == tj) {
          oi = true;
          break;
        }
      }
    }
    out[i] = oi;
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
LogicalVector do_par_in_hash_int(IntegerVector x, IntegerVector table, int nThread = 1) {
  std::unordered_set<int> H;
  int tn = table.length();
  for (int t = 0; t < tn; ++t) {
    H.insert(table[t]);
  }

  R_xlen_t N = x.length();
  LogicalVector out = no_init(N);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    int xi = x[i];
    bool oi = H.count(xi);
    out[i] = oi;
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
LogicalVector do_par_in_hash_dbl(DoubleVector x, DoubleVector table, int nThread = 1) {
  std::unordered_set<double> H;
  int tn = table.length();
  for (int t = 0; t < tn; ++t) {
    H.insert(table[t]);
  }

  R_xlen_t N = x.length();
  LogicalVector out = no_init(N);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    double xi = x[i];
    bool oi = H.count(xi);
    out[i] = oi;
  }
  return out;
}








