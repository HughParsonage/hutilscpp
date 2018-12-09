#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

//' @title Mode II
//' @param x Integer vector
//' @export ModeInt2
//' @name ModeInt2

// [[Rcpp::export]]
int ModeInt2(IntegerVector x, int xmax = -1, int xmin = 1) {
  int n = x.size();


  // Limit the search to the integer sequence min(x) .. max(x)
  // Use xmax > xmin as a proxy for 'the max and min are unknown'.
  if (xmin > xmax) {
    xmin = x[0];
    xmax = x[0];

    int xi = x[0];
    for (int i = 1; i < n; ++i) {
      xi = x[i];
      if (xmin > xi) {
        xmin = xi;
      } else {
        if (xmax < xi) {
          xmax = xi;
        }
      }
    }
  }
  int modec = 0;
  int mode = xmin;
  for (int j = xmin; j <= xmax; ++j) {
    int jc = 0;
    for (int k = 0; k < n; ++k) {
      if (x[k] == j) {
        ++jc;
      }
    }
    if (jc > modec) {
      modec = jc;
      mode = j;
    }

  }
  return mode;
}

