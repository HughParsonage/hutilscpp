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
Rcpp::IntegerMatrix texParse(LogicalVector open, LogicalVector close, int maxTeXGroup = 20) {
  int n = open.size();
  int nalt = close.size();
  if (n != nalt) {
    stop("open and close differ.");
  }
  IntegerVector texGroup(n);
  IntegerVector GROUP(n);
  int currentTeXGroup = 0;
  int currentGROUP = 0;
  IntegerMatrix out(n, maxTeXGroup + 1);
  for (int i = 0; i < n; ++i) {
    bool openi = open[i];
    bool closei = false;
    if (openi) {
      ++currentGROUP;
    } else {
      closei = close[i];
      if (closei) {
        --currentGROUP; // decrement after assignment
      }
    }
    for (int j = 1; j <= currentGROUP; ++j) {
      if (i == 0 || currentGROUP < 1) {
        continue;
      }
      int outij = out(i - 1, j);
      if (openi) {
        ++outij;
      }
      out(i, j) = outij;
    }


    if (currentGROUP > 0) {
      texGroup[i] = currentTeXGroup;
      GROUP[i] = currentGROUP;
      out(i, currentGROUP) = currentTeXGroup;
    }


  }
  return out;
}


