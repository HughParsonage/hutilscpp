#ifndef cpphutils_H
#define cpphutils_H

#ifdef _OPENMP
#include <omp.h>
#endif

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DoubleVector do_range_dbl(NumericVector x, double halt_if_min = 1, double halt_if_max = -1);

// [[Rcpp::export]]
DoubleVector do_range_int(IntegerVector x, int halt_if_min = 1, int halt_if_max = -1);

bool single_ox_x1_x2(int x, int oix, int x1, int x2);
bool single_ox_x1_x2(double x, int oix, double x1, double x2);

bool do_in_int(int x, IntegerVector table);

#endif
