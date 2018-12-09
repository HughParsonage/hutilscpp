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
int ModeInt(IntegerVector x) {
  int N = x.size();
  if (N <= 0) {
    return 0;
  }
  if (N <= 2) {
    // Either two distinct elements, or not; either way, the first is valid
    // (since by definition we only return one value)
    return x[0];
  }
  // candidate solution and alternate, with the sizes of each
  int out = x[0];
  int alt = x[1];
  int outn = 1;
  int altn = 0;

  int counted = 0;
  IntegerVector Checked(N);

  for (int i = N - 1; i >= 0; --i) {
    if (Checked[i] == 1) {
      continue;
    }
    out = x[i];
    outn = 1;
    for (int j = i - 1; j >= 0; --j) {
      if (out == x[j]) {
        Checked[j] = 1;
        ++outn;
        // if (verbose) Rprintf("\nduring\tout: %d\t", out);
        // if (verbose) Rprintf("outn: %d\t", outn);
        // if (verbose) Rprintf("alt: %d\t", alt);
        // if (verbose) Rprintf("altn: %d\t", altn);

        ++counted;
      }
    }
    //

    if (outn >= altn) {
      altn = outn;
      alt = out;
    }
    if (altn > N - counted + 1) {
      // if (verbose) Rprintf("altn = %d\n", altn);
      return alt;
    }

  }
  if (outn >= altn) { // outn is earlier than altn so prefer
    altn = outn;
    alt = out;
  } else {
    outn = altn;
    out = alt;
  }
  // if (verbose) Rprintf("\noutn = %d\n", outn);
  // if (verbose) Rprintf("altn = %d\n", altn);
  // if (verbose) Rprintf("alt  = %d\n", alt);
  return out;
}



