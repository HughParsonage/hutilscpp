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

// [[Rcpp::export]]
IntegerVector do_which_in(IntegerVector x, IntegerVector y) {
  int Count = 0;
  int yn = y.length();
  int xn = x.length();
  for (int i = 0; i < xn; ++i) {
    for (int j = 0; j < yn; ++j) {
      if (x[i] == y[j]) {
        Count++;
        break;
      }
    }
  }

  IntegerVector out(Count);
  int k = 0;
  for (int i = 0; i < xn; ++i) {
    for (int j = 0; j < yn; ++j) {
      if (x[i] == y[j]) {
        out[k] = i + 1;
        ++k;
        break;
      }
    }
  }
  return out;

}
