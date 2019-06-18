#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector do_or3(LogicalVector x, LogicalVector y, LogicalVector z) {
  R_xlen_t N = x.length();
  if (y.length() != N) {
    stop("y and x have different lengths.");
  }
  LogicalVector out(N);
  if (z.length() != N) {
    if (z.length() > 1) {
      stop("z has the wrong length");
    }
    if (z.length() == 0) {
      for (R_xlen_t i = 0; i < N; ++i) {
        out[i] = x[i] || y[i];
      }
    } else {
     if (z[0]) {
       for (R_xlen_t i = 0; i < N; ++i) {
         out[i] = true;
       }
     } else {
       for (R_xlen_t i = 0; i < N; ++i) {
         out[i] = x[i] || y[i];
       }
     }

    }
  } else {
    for (R_xlen_t i = 0; i < N; ++i) {
      out[i] = x[i] || y[i] || z[i];
    }
  }
  return out;
}

// [[Rcpp::export]]
LogicalVector do_and3(LogicalVector x, LogicalVector y, LogicalVector z) {
  R_xlen_t N = x.length();
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
      for (R_xlen_t i = 0; i < N; ++i) {
        out[i] = x[i] && y[i];
      }
    } else {
      // z = false so all are false
      return out;


    }
  } else {
    for (R_xlen_t i = 0; i < N; ++i) {
      out[i] = x[i] && y[i] && z[i];
    }
  }
  return out;
}

// [[Rcpp::export]]
LogicalVector na_and (LogicalVector x) {
  // NA & x
  R_xlen_t n = x.length();
  LogicalVector out(n);
  for (R_xlen_t i = 0; i < n; ++i) {
    if (x[i] != FALSE) {
      out[i] = NA_LOGICAL;
    }
  }
  return out;
}

// [[Rcpp::export]]
List do_which3(LogicalVector x, LogicalVector y, LogicalVector z, bool And = true) {
  R_xlen_t n = (x.length() > 1) ? x.length() : ((y.length() > 1) ? y.length() : z.length());
  const bool nx = x.length() == n;
  const bool ny = y.length() == n;
  const bool nz = z.length() == n;

  IntegerVector out(n);
  int j = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    int xi = nx ? x[i] : x[0];
    int yi = ny ? y[i] : y[0];
    int zi = nz ? z[i] : z[0];

    if (And) {
      if (xi != NA_LOGICAL && xi &&
          yi != NA_LOGICAL && yi &&
          zi != NA_LOGICAL && zi) {
        out[j] = i + 1;
        ++j;
      }
    } else {
      if ((xi != NA_LOGICAL && xi) ||
          (yi != NA_LOGICAL && yi) ||
          (zi != NA_LOGICAL && zi)) {
        out[j] = i + 1;
        ++j;
      }
    }
  }
  return List::create(j, out);
}

// [[Rcpp::export]]
IntegerVector do_which3_mem(LogicalVector x, LogicalVector y, LogicalVector z, bool And = true) {
  R_xlen_t n = (x.length() > 1) ? x.length() : ((y.length() > 1) ? y.length() : z.length());
  const bool nx = x.length() == n;
  const bool ny = y.length() == n;
  const bool nz = z.length() == n;



  R_xlen_t Count = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    int xi = nx ? x[i] : x[0];
    int yi = ny ? y[i] : y[0];
    int zi = nz ? z[i] : z[0];
    if (xi && yi && zi) {
      ++Count;
    }
  }
  IntegerVector out(Count);
  int j = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    int xi = nx ? x[i] : x[0];
    int yi = ny ? y[i] : y[0];
    int zi = nz ? z[i] : z[0];
    if (xi && yi && zi) {
      out[j] = i + 1;
      ++j;
    }
  }
  return out;
}

void showValuei(const char* what, int x) {
  Rcout << " " << what << " \t " << x << std::endl;
  // return 0;
}

// [[Rcpp::export]]
IntegerVector do_which3_prepare(IntegerVector wx, IntegerVector wy, IntegerVector wz) {
  int wxn = wx.size();
  int wyn = wy.size();
  int wzn = wz.size();

  const bool usex = wxn <= wyn && wxn <= wzn;
  const bool usey = !usex && wyn <= wzn;
  const bool usez = !usex && !usey;
  if (usey) {
    stop("y too long.");
  }
  if (usez) {
    stop("z too long.");
  }

  // Choose the smallest of the w's (since we're only considering the intersection)
  IntegerVector out(clone(wx));

  int N = wx.size();

  int i = N;
  int j = wyn;
  int k = wzn;


  while (i >= 1) {
    --i;
    int oi = wx[i];
    while (j >= 1) {
      --j;
      int wyj = wy[j];
      if (oi == wyj) {
        while (k >= 1) {
          --k;
          int wzk = wz[k];
          if (oi == wzk) {
            break;
          } else {
            if (oi > wzk || k == 0) {
              out.erase(i);
              break;
            }
          }
        }
        break;
      } else {
        if (oi > wyj || j == 0) {
          out.erase(i);
          break;
        }
      }

    }
  }

  return out;

}

// [[Rcpp::export]]
IntegerVector do_which3_prepare1(IntegerVector wx,
                                 LogicalVector y,
                                 LogicalVector z) {
  int N = wx.size();
  IntegerVector out(clone(wx));

  int Ny = y.size();
  int Nz = z.size();
  if (Ny < wx[N - 1] || Nz < wx[N - 1]) {
    stop("Unexpected length.");
  }

  int j = N - 1;
  for (int i = N - 1; i >= 0; --i) {
    // i is an index of an index
    // j is the actual candidate index of y or z
    j = wx[i] - 1;
    if (y[j] != TRUE || z[j] != TRUE) {
      out.erase(i);
    }
  }
  return out;
}


// [[Rcpp::export]]
IntegerVector EraseTest(IntegerVector x, LogicalVector y) {
  int i = x.size();
  IntegerVector out(clone(x));
  while (i >= 1) {
    --i;
    if (y[i]) {
      out.erase(i);
    }

  }
  return out;
}




