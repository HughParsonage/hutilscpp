#include <Rcpp.h>
using namespace Rcpp;
#include "cpphutils.h"


IntegerVector do_range_int(IntegerVector x, int halt_if_min, int halt_if_max) {
  R_xlen_t n = x.size();
  int maxi = x[n - 1];
  int which_max = n - 1;
  int mini = x[0];
  int which_min = 0;
  const bool do_halt = halt_if_min < halt_if_max;
  if (do_halt) {
    maxi = x[0];
    which_max = 0;
  }

  for (R_xlen_t i = 0; i < n; ++i) {
    int xi = x[i];
    if (xi == NA_INTEGER) {
      continue;
    }
    if (xi < mini) {
      mini = xi;
      which_min = i;
    } else {
      if (xi > maxi) {
        maxi = xi;
        which_max = i;
      }
    }
    if (do_halt) {
      if (mini <= halt_if_min ||
          maxi >= halt_if_max) {
        break;
      }
    }
  }
  IntegerVector out(4);
  ++which_min;
  ++which_max;
  out[0] = mini;
  out[1] = maxi;
  out[2] = which_min;
  out[3] = which_max;

  return out;

}



DoubleVector do_range_dbl(DoubleVector x, double halt_if_min, double halt_if_max) {
  R_xlen_t n = x.size();
  double maxi = x[n - 1];
  double which_max = n - 1;
  double mini = x[0];
  double which_min = 0;
  const bool do_halt = halt_if_min < halt_if_max;
  if (do_halt) {
    maxi = x[0];
    which_max = 0;
  }
  for (R_xlen_t i = 0; i < n; ++i) {
    double xi = x[i];
    if (xi == NA_REAL) {
      continue;
    }
    if (xi < mini) {
      mini = xi;
      which_min = i;
    } else {
      if (xi > maxi) {
        maxi = xi;
        which_max = i;
      }
    }
    if (do_halt) {
      if (mini <= halt_if_min ||
          maxi >= halt_if_max) {
        break;
      }
    }
  }
  ++which_min;
  ++which_max;
  DoubleVector out(4);
  out[0] = mini;
  out[1] = maxi;
  out[2] = which_min;
  out[3] = which_max;
  return out;

}
