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

// #nocov start
// [[Rcpp::export]]
void showValuea(const char* what, double x) {
  Rcout << "The value " << what << " is " << x << std::endl;
}
// #nocov end


// [[Rcpp::export]]
List extractMandatory (CharacterVector x, CharacterVector command, int nCommands) {
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

  CharacterVector support(N);
  IntegerVector commandNo(nCommands);
  IntegerVector commandOpeners(nCommands);
  IntegerVector commandClosers(nCommands);
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
          if (x[k] == " " || x[k] == "") {
            // \abc {def}
            continue;
          }
          if (x[k] == "[") {
            ++opt_group;
            int rel_opt_group = 1;
            while (rel_opt_group && k < N) {
              // just keep moving forward until we get out of the current
              // optional group.
              ++k;

              if (x[k] == "]") {
                --rel_opt_group;
                --opt_group;
              } else {
                if (x[k] == "[") {
                  ++rel_opt_group;
                  ++opt_group;
                }
              }
            }
            // Current position is on the closing ']'
            ++k; // move to next character once we're done with optional group
          }
          if (k >= N) { // in case the document is not well-formed.
            break;
          }
          within_brace = x[k] == "{";

          // abc{xyz} but not abcd{xyz}
          if (x[k] != "" && x[k] != " " && x[k] != "{") {
            k = N + 1;  // effectively break both while loops
          } else {
            commandOpeners[command_no] = k;
          }
        }

        while (k < N) {
          support[k] = x[k];
          if (command_no >= nCommands) {
            showValuea("command_no = ", command_no);
            showValuea("k = ", k);
            stop("command_no overflow");
          }
          // R indexing
          commandNo[command_no] = command_no + 1;


          finish_extract = x[k] == "}";
          if (finish_extract) {
            commandClosers[command_no] = k + 1;
            ++command_no;
            break;
          }
          ++k;
        }
      }
    }
  }

  List out = List::create(Named("support") = support,
                          Named("Openers") = commandOpeners,
                          Named("Closers") = commandClosers);


  return out;
}



