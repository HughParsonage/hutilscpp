#include <Rcpp.h>
using namespace Rcpp;

//' @name helper1
//' @title Helper
//' @param x,y,miny,maxy Inputs.

// [[Rcpp::export]]
int helper1(NumericVector x, NumericVector y, double miny, double maxy) {
  int N = x.size();
  int M = y.size();
  int o = 0;
  for (int i = 0; i < N; ++i) {
    double xi = x[i];
    for (int j = 0; j < M; ++j) {
      double yi = y[i];
      if (yi > miny && yi < maxy) {
        o = 0;
        o += j;
      }
    }
  }
  return o;
}


