#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector do_range_int (IntegerVector x) {
  int n = x.size();
  int maxi = x[n - 1];
  int mini = x[0];
  if (maxi < mini) {
    mini = maxi;
    maxi = x[0];
  }
  for (int i = 1; i < n; ++i) {
    int xi = x[i];
    if (xi < mini) {
      mini = xi;
    } else {
      if (xi > maxi) {
        maxi = xi;
      }
    }
  }
  IntegerVector out(2);
  out[0] = mini;
  out[1] = maxi;
  return out;

}

#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
DoubleVector do_range_dbl (DoubleVector x) {
  int n = x.size();
  double maxi = x[n - 1];
  double mini = x[0];
  if (maxi < mini) {
    mini = maxi;
    maxi = x[0];
  }
  for (int i = 1; i < n; ++i) {
    double xi = x[i];
    if (xi < mini) {
      mini = xi;
    } else {
      if (xi > maxi) {
        maxi = xi;
      }
    }
  }
  DoubleVector out(2);
  out[0] = mini;
  out[1] = maxi;
  return out;

}
