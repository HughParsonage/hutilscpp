#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
int do_which_true_onwards(LogicalVector x) {
  int N = x.size();
  int out = N - 1;
  if (!x[out]) {
    return 0;
  }
  while (x[out] && out >= 0) {
    --out;
  }
  return out + 2; // out now at FALSE (or -1), + 1 for TRUE, + 1 for 0-index
}


