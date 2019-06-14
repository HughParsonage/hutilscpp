#include <Rcpp.h>
using namespace Rcpp;

// #nocov start
int showValuea(const char* what, double x) {
  // Rcout << "The value " << what << " is " << x << std::endl;
  return 0;
}
// #nocov end


// [[Rcpp::export]]
List extractMandatory (CharacterVector x, CharacterVector command, int nCommands) {
  R_xlen_t N = x.length();
  int command_len = command.length();
  // const char *xp = x[0];
  // const char *cp = command[0];

  bool finish_extract = false;
  bool within_brace = false;
  // bool within_optional = false;
  int opt_group = 0;
  int cj = 0;


  int command_no = 0;

  CharacterVector support(N);
  IntegerVector commandNo(nCommands);
  IntegerVector commandOpeners(nCommands);
  IntegerVector commandClosers(nCommands);
  for (R_xlen_t i = 0; i < N; ++i) {
    int k = 0;
    if (x[i] == command[0]) {
      showValuea("k", k);
      for (int ci = 1; ci < command_len; ++ci) {
        // Consider command = \abc and document ends with ab
        if (i + ci >= N) {
          break;
        }
        const char *cii = command[ci];
        const char *xic = x[i + ci];
        // if (x[i + ci] == command[ci]) {
        showValuea("i + ci", i + ci);
        showValuea("cj", cj);
        if (*xic == *cii) {
          ++cj;
        } else {
          cj = 0;
          break;
        }
      }
      // Zero if at the same group level as the command name
      int rel_group = 0;
      if (cj == command_len - 1) {
        // Now wait until we see an opening brace
        within_brace = false;
        finish_extract = false;
        k = i + cj;
        while (!within_brace && k < N - 1) {
          showValuea("++k1", k);
          ++k;
          if (x[k] == " " || x[k] == "") {
            // \abc {def}
            continue;
          }
          if (x[k] == "[") {
            ++opt_group;
            int rel_opt_group = 1;
            while (rel_opt_group && k < N - 1) {
              // just keep moving forward until we get out of the current
              // optional group.
              ++k;
              showValuea("++k1_rel_group", k);

              if (x[k] == "]") {
                --rel_opt_group;
                --opt_group;
              } else {
                if (x[k] == "{") {
                  ++rel_group;
                  while (rel_group && k < N - 1) {
                    ++k;
                    if (x[k] == "}") {
                      --rel_group;
                    } else {
                      if (x[k] == "{") {
                        ++rel_group;
                      }
                    }
                  }
                }
                if (x[k] == "[") {
                  ++rel_opt_group;
                  ++opt_group;
                }
              }
            }
            showValuea("++k2", k);
            // Current position is on the closing ']'
            ++k; // move to next character once we're done with optional group
          }
          if (k >= N) { // in case the document is not well-formed.
            break;
          }
          within_brace = (rel_group == 0) && x[k] == "{";

          // abc{xyz} but not abcd{xyz}
          if (x[k] != "" && x[k] != " " && x[k] != "{") {
            showValuea("x[k]", k);
            k = N + 1;  // effectively break both while loops
          } else {
            commandOpeners[command_no] = k + 1;
          }
        }

        showValuea("at while(k < N), k", k);
        showValuea("at while(k < N), N", N);
        while (k < N) {
          support[k] = x[k];
          // #nocov start
          if (command_no >= nCommands) {
            showValuea("command_no = ", command_no);
            showValuea("k = ", k);
            stop("command_no overflow");
          }
          // #nocov end
          // R indexing
          commandNo[command_no] = command_no + 1;
          if (x[k] == "{") {
            ++rel_group;
          } else {
            if (x[k] == "}") {
              --rel_group;
            }
          }


          finish_extract = (rel_group == 0) && x[k] == "}";
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



