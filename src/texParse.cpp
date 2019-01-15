#include <Rcpp.h>
using namespace Rcpp;

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




// [[Rcpp::export]]
CharacterVector extractMandatory (CharacterVector x, CharacterVector command) {
  int N = x.length();
  int command_len = command.length();
  const char *xp = x[0];
  const char *cp = command[0];

  const char *c1 = cp;

  int curr_group = 0;
  bool finish_extract = false;
  bool within_brace = false;
  bool within_optional = false;
  int opt_group = 0;
  int cj = 0;
  int k = 0;

  int command_no = 0;

  CharacterVector out(N);
  for (int i = 0; i < N; ++i) {
    if (x[i] == command[0]) {
      for (int ci = 1; ci < command_len; ++ci) {
        const char *cii = command[ci];
        const char *xic = x[i + ci];
        if (x[i + ci] == command[ci]) {
          ++cj;
          continue;
        } else {
          cj = 0;
          break;
        }
      }
      if (cj == command_len - 1) {
        // Now wait until we see an opening brace
        within_brace = false;
        finish_extract = false;
        k = i + cj;

        while (!within_brace && k < N) {
          ++k;
          if (x[k] == "[") {
            ++opt_group;
            int rel_opt_group = 1;
            int kkkk = 0;
            while (rel_opt_group && k < N && kkkk < N) {
              ++kkkk;
              if (x[k] == "]") {
                --rel_opt_group;
                --opt_group;
              } else {
                if (x[k] == "[") {
                  ++rel_opt_group;
                  ++opt_group;
                }
              }
              ++k ;
            }
          }
          within_brace = x[k] == "{";

          // abc{xyz} but not abcd{xyz}
          if (x[k] != "" && x[k] != " " && x[k] != "{") {
            k = N + 1;  // effectively break both while loops
          }
        }
        ++command_no ;

        while (k < N) {
          out[k] = x[k];
          finish_extract = x[k] == "}";
          if (finish_extract) {
            break;
          }
          ++k;
        }
      }
    }
  }
  return out;
}



