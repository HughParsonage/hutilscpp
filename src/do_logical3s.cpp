#include "cpphutils.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(rng = false)]]
LogicalVector do_and3_na(LogicalVector x, LogicalVector y, LogicalVector z,
                         int nThread = 1,
                         int na_value = 0,
                         int maxCall = 3) {
  if (maxCall < 0) {
    stop("Internal error: maxCall exceeded.");
  }

  // length-0 = ignore
  // length-1 = treat as if it were recycled: TRUE => ignore; FALSE => all FALSE; NA => NA.
  R_xlen_t nx = x.length();
  R_xlen_t ny = y.length();
  R_xlen_t nz = z.length();

  R_xlen_t n = (nx >= ny && nx >= nz) ? nx : ((ny >= nz) ? ny : nz);
  if (n == 0) {
    return x;
  }
  LogicalVector out = no_init(n);

  bool na_becomes_false = na_value == 0;
  bool na_becomes_true  = na_value > 0;
  bool na_becomes_na    = na_value < 0;


  if (nx == n && ny == n && nz == n) {
#pragma omp parallel for num_threads(nThread)
    for (R_xlen_t i = 0; i < n; ++i) {
      if (na_becomes_false) {
        out[i] = (x[i] == TRUE) && (y[i] == TRUE) && (z[i] == TRUE);
      }
      if (na_becomes_true) {
        out[i] = !(x[i] == FALSE || y[i] == FALSE || z[i] == FALSE);
      }
      if (na_becomes_na) {
        if (x[i] == NA_LOGICAL || y[i] == NA_LOGICAL || z[i] == NA_LOGICAL) {
          out[i] = NA_LOGICAL;
        } else {
          out[i] = x[i] && y[i] && z[i];
        }
      }
    }
    return out;
  }
  // at this point one of the values is not n (and not zero)
  // error if any not a good value
  if ((nx > 1 && nx < n) ||
      (ny > 1 && ny < n) ||
      (nz > 1 && ny < n)) {
    stop("Internal error nx, ny, nz have incompatible/wrong lengths.");
  }

  // ensure lengths are non-increasing
  if (n > nx) {
    if (ny >= nz) {
      return do_and3_na(y, x, z, nThread, --maxCall);
    } else {
      return do_and3_na(z, y, x, nThread, --maxCall);
    }
  }
  if (nx > ny) {
    return do_and3_na(x, z, y, nThread, --maxCall);
  }

  // so they must all have 0, 1, n lengths
  // we know that nx is maximal so it can be ignored
  if (ny == 0 && nz == 0) {
    return x;
  }
  if (nz == 0) {
    // ignore nz
    return do_and3_na(x, y, y, nThread, --maxCall);
  }

  if (ny == 1 && nz == 1) {
    // y or z must be FALSE or NA
    if (y[0] == FALSE || z[0] == FALSE) {
      return LogicalVector(n);
    }
    if (y[0] == TRUE && z[0] == TRUE) {
      return x;
    }
    if (na_becomes_true) {
      return x;
    }
    if (y[0] == NA_LOGICAL || z[0] == NA_LOGICAL) {
      for (R_xlen_t i = 0; i < n; ++i) {
        if (na_becomes_false) {
          out[i] = FALSE;
        }
        if (na_becomes_na) {
          out[i] = NA_LOGICAL;
        }
      }
      return out;
    }
  }
  if (ny == 1) {
    if (y[0] == FALSE) {
      return LogicalVector(n);
    }
    if (y[0] == TRUE || na_becomes_true) {
      return x;
    }
    if (y[0] == NA_LOGICAL) {
      for (R_xlen_t i = 0; i < n; ++i) {
        if (na_becomes_false) {
          out[i] = FALSE;
        }
        if (na_becomes_na) {
          out[i] = NA_LOGICAL;
        }
      }
      return out;
    }
  }

  // nx = ny

  if (nz == 1) {
    if (z[0] == TRUE || na_becomes_true) {
      return do_and3_na(x, y, y, nThread, --maxCall);
    }
    if (z[0] == FALSE || na_becomes_false) {
      return LogicalVector(n);
    }
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] = NA_LOGICAL;
    }
    return out;
  }


  stop("Internal error: Should be unreachable.");
  return out;
}

inline int opn(bool eq, bool gt, bool lt) {
  // != == >= <=  >  <
  //  0  1  2  3  4  5
  return !(eq || gt || lt) ? 0 : (eq ? (gt ? 2 : (lt ? 3 : 1)) : (gt ? 4 : 5));
}


// [[Rcpp::export(rng = false)]]
IntegerVector do_which2_yr(IntegerVector Year,
                           int yr,
                           bool consider_yr,
                           IntegerVector x,
                           int xa,
                           bool eqx,
                           bool gtx,
                           bool ltx,
                           IntegerVector y,
                           int ya,
                           bool eqy,
                           bool gty,
                           bool lty) {
  int nx = x.size();
  if (y.size() != nx) {
    stop("Internal error: lengths differ.");
  }
  if ((gtx && ltx) ||
      (gty && lty)) {
    stop("Internal error: both > and < requested.");
  }
  std::vector<int> o;
  o.reserve(nx);
  const int opx = opn(eqx, gtx, ltx);
  const int opy = opn(eqy, gty, lty);
  const int opxy = opx + 10*opy;
  if (consider_yr && Year.size() != nx) {
    stop("Internal error: lengths differ (Year).");
  }

  for (int i = 0; i < nx; i++) {
    bool yr_bool = true;
    if (consider_yr) {
      yr_bool = Year[i] == yr;
    }
    switch (opxy) {
    case 0:
      if (yr_bool && x[i] != xa && y[i] != ya) o.push_back(i + 1);
      continue;
    case 1:
      if (yr_bool && x[i] == xa && y[i] != ya) o.push_back(i + 1);
      continue;
    case 2:
      if (yr_bool && x[i] >= xa && y[i] != ya) o.push_back(i + 1);
      continue;
    case 3:
      if (yr_bool && x[i] <= xa && y[i] != ya) o.push_back(i + 1);
      continue;
    case 4:
      if (yr_bool && x[i] >  xa && y[i] != ya) o.push_back(i + 1);
      continue;
    case 5:
      if (yr_bool && x[i] <  xa && y[i] != ya) o.push_back(i + 1);
      continue;

    case 10:
      if (yr_bool && x[i] != xa && y[i] == ya) o.push_back(i + 1);
      continue;
    case 11:
      if (yr_bool && x[i] == xa && y[i] == ya) o.push_back(i + 1);
      continue;
    case 12:
      if (yr_bool && x[i] >= xa && y[i] == ya) o.push_back(i + 1);
      continue;
    case 13:
      if (yr_bool && x[i] <= xa && y[i] == ya) o.push_back(i + 1);
      continue;
    case 14:
      if (yr_bool && x[i] >  xa && y[i] == ya) o.push_back(i + 1);
      continue;
    case 15:
      if (yr_bool && x[i] <  xa && y[i] == ya) o.push_back(i + 1);
      continue;

    case 20:
      if (yr_bool && x[i] != xa && y[i] >= ya) o.push_back(i + 1);
      continue;
    case 21:
      if (yr_bool && x[i] == xa && y[i] >= ya) o.push_back(i + 1);
      continue;
    case 22:
      if (yr_bool && x[i] >= xa && y[i] >= ya) o.push_back(i + 1);
      continue;
    case 23:
      if (yr_bool && x[i] <= xa && y[i] >= ya) o.push_back(i + 1);
      continue;
    case 24:
      if (yr_bool && x[i] >  xa && y[i] >= ya) o.push_back(i + 1);
      continue;
    case 25:
      if (yr_bool && x[i] <  xa && y[i] >= ya) o.push_back(i + 1);
      continue;

    case 30:
      if (yr_bool && x[i] != xa && y[i] <= ya) o.push_back(i + 1);
      continue;
    case 31:
      if (yr_bool && x[i] == xa && y[i] <= ya) o.push_back(i + 1);
      continue;
    case 32:
      if (yr_bool && x[i] >= xa && y[i] <= ya) o.push_back(i + 1);
      continue;
    case 33:
      if (yr_bool && x[i] <= xa && y[i] <= ya) o.push_back(i + 1);
      continue;
    case 34:
      if (yr_bool && x[i] >  xa && y[i] <= ya) o.push_back(i + 1);
      continue;
    case 35:
      if (yr_bool && x[i] <  xa && y[i] <= ya) o.push_back(i + 1);
      continue;

    case 40:
      if (yr_bool && x[i] != xa && y[i] > ya) o.push_back(i + 1);
      continue;
    case 41:
      if (yr_bool && x[i] == xa && y[i] > ya) o.push_back(i + 1);
      continue;
    case 42:
      if (yr_bool && x[i] >= xa && y[i] > ya) o.push_back(i + 1);
      continue;
    case 43:
      if (yr_bool && x[i] <= xa && y[i] > ya) o.push_back(i + 1);
      continue;
    case 44:
      if (yr_bool && x[i] >  xa && y[i] > ya) o.push_back(i + 1);
      continue;
    case 45:
      if (yr_bool && x[i] <  xa && y[i] > ya) o.push_back(i + 1);
      continue;

    case 50:
      if (yr_bool && x[i] != xa && y[i] < ya) o.push_back(i + 1);
      continue;
    case 51:
      if (yr_bool && x[i] == xa && y[i] < ya) o.push_back(i + 1);
      continue;
    case 52:
      if (yr_bool && x[i] >= xa && y[i] < ya) o.push_back(i + 1);
      continue;
    case 53:
      if (yr_bool && x[i] <= xa && y[i] < ya) o.push_back(i + 1);
      continue;
    case 54:
      if (yr_bool && x[i] >  xa && y[i] < ya) o.push_back(i + 1);
      continue;
    case 55:
      if (yr_bool && x[i] <  xa && y[i] < ya) o.push_back(i + 1);
      continue;
    }
  }
  return wrap(o);
}


// [[Rcpp::export(rng = false)]]
IntegerVector do_which_in(IntegerVector x, IntegerVector tbl) {
  R_xlen_t n = x.size();
  if (n >= INT_MAX) {
    stop("Length exceeds integer max.");
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


// [[Rcpp::export(rng = false)]]
IntegerVector count_logical(LogicalVector x) {
  int n = x.length();
  int trues = 0;
  int nas = 0;
#pragma omp parallel for reduction(+:trues,nas)
  for (int i = 0; i < n; ++i) {
    if (x[i] == NA_LOGICAL) {
      nas += 1;
    } else if (x[i]) {
      trues += 1;
    }
  }
  int falses = n - trues - nas;

  IntegerVector o(3);
  o[0] = falses;
  o[1] = trues;
  o[2] = nas;
  return o;
}


bool single_ox_x1_x2(int x, int oix, int x1, int x2) {
  //  T != == >= <=  >  <
  //  0  1  2  3  4  5  6
  switch(oix) {
  case 1:
    return x != x1;
  case 2:
    return x == x1;
  case 3:
    return x >= x1;
  case 4:
    return x <= x1;
  case 5:
    return x >  x1;
  case 6:
    return x <  x1;
  case 7:
    return x == x1 || x == x2;
  case 8:
    return x >= x1 && x <= x2;
  case 9:
    return x >  x1 && x <  x2;
  case 10:
    return x <= x1 && x <= x2;
  case 0:
    return true;
  }
  return false;
}

// [[Rcpp::export(rng = false)]]
bool do_in_int(int x, IntegerVector table) {
  int tn = table.length();
  for (int i = 0; i < tn; ++i) {
    if (x == table[i]) {
      return true;
    }
  }
  return false;
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

#pragma omp parallel for num_threads(nThread) shared(out, tango)
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
#pragma omp parallel for num_threads(nThread)
  for (R_xlen_t i = 0; i < N; ++i) {
    int xi = x[i];
    bool oi = H.count(xi);
    out[i] = oi;
  }
  return out;

}


// [[Rcpp::export(rng = false)]]
LogicalVector do_and3_x_op(IntegerVector x, int ox, int x1, int x2,
                           IntegerVector y, int oy, int y1, int y2,
                           IntegerVector z, int oz, int z1, int z2,
                           IntegerVector X3,
                           IntegerVector Y3,
                           IntegerVector Z3,
                           LogicalVector A,
                           LogicalVector B,
                           LogicalVector C,
                           int nThread = 1) {
  R_xlen_t xn = x.length();
  R_xlen_t An = A.length();
  if (An > 1 && xn > 1) {
    stop("Internal error: length(A) != length(x).");
  }
  R_xlen_t n = (An > 1) ? An : xn;

  bool ignore_z = z.length() <= 1 && C.length() <= 1;
  if ((n != y.length() && n != B.length()) ||
      (!ignore_z && n != z.length() && n != C.length())) {
    stop("Internal error: lengths differ.");
  }
  LogicalVector out = no_init(n);

  const int x3n = X3.length();
  const int y3n = Y3.length();
  const int z3n = Z3.length();

  // Which variables are lhs %in% rhs
  const bool x_in = x3n > 1 && ox == 7;
  const bool y_in = y3n > 1 && oy == 7;
  const bool z_in = z3n > 1 && oz == 7;

  // Which variables are bare logicals
  const bool A_lgl = A.length() == n;
  const bool B_lgl = B.length() == n;
  const bool C_lgl = C.length() == n;

  if (x3n == 2 && !y_in && !z_in && !ignore_z) {
    int X30 = X3[0];
    int X31 = X3[1];

#pragma omp parallel for num_threads(nThread)
    for (R_xlen_t i = 0; i < n; ++i) {
      int xi = x[i];
      bool oi =
        (xi == X30 || xi == X31) &&
        (B_lgl ? B[i] : single_ox_x1_x2(y[i], oy, y1, y2)) &&
        (ignore_z ||
        (C_lgl ? C[i] : single_ox_x1_x2(z[i], oz, z1, z2)));
      out[i] = oi;
    }
    return out;

  } else if (x3n == 3 && !y_in && !z_in) {
    int X30 = X3[0];
    int X31 = X3[1];
    int X32 = X3[2];

#pragma omp parallel for num_threads(nThread)
    for (R_xlen_t i = 0; i < n; ++i) {
      int xi = x[i];
      bool oi =
        (xi == X30 || xi == X31 || xi == X32) &&
        (B_lgl ? B[i] : single_ox_x1_x2(y[i], oy, y1, y2)) &&
        (ignore_z ||
        (C_lgl ? C[i] : single_ox_x1_x2(z[i], oz, z1, z2)));
      out[i] = oi;
    }
    return out;

  } else if ((x_in || y_in || z_in) && !ignore_z) {

// do in int not thread-safe
    for (R_xlen_t i = 0; i < n; ++i) {
      bool oi =
        ((x_in) ? do_in_int(x[i], X3) : (A_lgl ? A[i] : single_ox_x1_x2(x[i], ox, x1, x2))) &&
        ((y_in) ? do_in_int(y[i], Y3) : (B_lgl ? B[i] : single_ox_x1_x2(y[i], oy, y1, y2))) &&
        ((z_in) ? do_in_int(z[i], Z3) : (C_lgl ? C[i] : single_ox_x1_x2(z[i], oz, z1, z2)));
      out[i] = oi;
    }
  } else {
#pragma omp parallel for num_threads(nThread)
    for (R_xlen_t i = 0; i < n; ++i) {
      bool oi =
        (A_lgl ? A[i] : single_ox_x1_x2(x[i], ox, x1, x2)) &&
        (B_lgl ? B[i] : single_ox_x1_x2(y[i], oy, y1, y2)) &&
        (ignore_z ||
        (C_lgl ? C[i] : single_ox_x1_x2(z[i], oz, z1, z2)));
      out[i] = oi;
    }
  }

  return out;
}

// [[Rcpp::export]]
LogicalVector do_op_along(IntegerVector x, int op, IntegerVector y, int nThread = 1) {
  R_xlen_t N = x.length();
  if (y.length() != N) {
    stop("Internal error: do_op_along() lengths differ.");
  }
  LogicalVector out = no_init(N);
#pragma omp parallel for num_threads(nThread)
  for (R_xlen_t i = 0; i < N; ++i) {
    out[i] = single_ox_x1_x2(x[i], op, y[i], 0);
  }
  return out;
}






