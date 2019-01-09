#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int AnyCharMatch(CharacterVector x, CharacterVector a) {
  // const char ac = a[0];
  const char *acp = a[0];
  for (int i = 0; i < x.size(); ++i) {
    const char *xcp = x[i];
    if (std::strcmp(xcp, acp) == 0) {
      return ++i;
    }
  }
  return 0;
}



