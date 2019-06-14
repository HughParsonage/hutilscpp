#include <Rcpp.h>
#include <Rinternals.h>
using namespace Rcpp;

// [[Rcpp::export]]
int AnyCharMatch(CharacterVector x, CharacterVector a, bool opposite = false) {
  // const char ac = a[0];
  const char *acp = a[0];
  const bool a_is_na = R_IsNA(*acp);
  const R_xlen_t n = x.size();
  for (R_xlen_t i = 0; i < n; ++i) {
    const char *xcp = x[i];
    if (opposite) {
      if (a_is_na) {
        if (!R_IsNA(*xcp)) {
          return ++i;
        }
      } else {
        if (xcp != acp) {
          return ++i;
        }
      }
    } else {
      if (a_is_na) {
        if (R_IsNA(*xcp)) {
          return ++i;
        }
      } else {
        if (xcp == acp) {
          return ++i;
        }
      }
    }
  }
  return 0;
}



