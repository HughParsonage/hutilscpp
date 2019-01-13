// We can now use the BH package
// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <boost/lexical_cast.hpp>
#include "lexiCast.h"

using namespace Rcpp;

using boost::lexical_cast;
using boost::bad_lexical_cast;

// This function is needed because std::to_string doesn't work on Windows:
// https://stackoverflow.com/questions/12975341/to-string-is-not-a-member-of-std-says-g-mingw

// source: https://stackoverflow.com/questions/49015611/convert-individual-rcppintegervector-element-to-a-character

// [[Rcpp::export]]
std::vector<std::string> lexicalCast(std::vector<int> v) {

  std::vector<std::string> res(v.size());

  for (unsigned int i=0; i<v.size(); i++) {
    try {
      res[i] = lexical_cast<std::string>(v[i]);
    } catch(bad_lexical_cast &) {
      res[i] = "(failed)";
    }
  }

  return res;
}
