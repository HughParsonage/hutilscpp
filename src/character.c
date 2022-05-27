#include "hutilscpp.h"

const char SPACE = 32;
const char OPEN_BRACE = 123;
const char STOP_BRACE = 125;
const char OPEN_SQBRK = 91;
const char STOP_SQBRK = 93;

bool string_equal(const char * x, const char * y) {
  // return x == y; // this may be sufficient
  if (x == y) {
    return true;
  }
  if (x[0] == '\0') {
    return y[0] == '\0';
  }
  int i = 0;
  while (x[i] != '\0' && y[i] != '\0') {
    if (x[i] != y[i]) {
      return false;
    }
    ++i;
  }
  return x[i] == y[i];
}

bool string_equaln(const char * x, int nx, const char * y) {
  if (nx == 0) {
    return y[0] == '\0';
  }
  for (int i = 0; i < nx; ++i) {
    if (y[i] == '\0' || x[i] != y[i]) {
      return false;
    }
  }
  return true;
}

SEXP CStringEqual(SEXP x, SEXP y) {
  if (TYPEOF(x) != STRSXP || TYPEOF(y) != STRSXP) {
    return ScalarLogical(0); // # nocov
  }
  if (xlength(y) == 1) {
    const char * y0 = CHAR(STRING_ELT(y, 0));
    int ny = strlen(y0);
    R_xlen_t N = xlength(x);
    for (R_xlen_t i = 0; i < N; ++i) {
      const char * xi = CHAR(STRING_ELT(x, i));
      if (!string_equaln(y0, ny, xi)) {
        return ScalarLogical(0);
      }
    }
    return ScalarLogical(1);
  }
  if (TYPEOF(x) != STRSXP || TYPEOF(y) != STRSXP ||
      xlength(x) != xlength(y)) {
    return ScalarLogical(0);
  }
  R_xlen_t N = xlength(x);
  const SEXP * xp = STRING_PTR(x);
  const SEXP * yp = STRING_PTR(y);
  for (R_xlen_t i = 0; i < N; ++i) {
    const char * xi = CHAR(xp[i]);
    const char * yi = CHAR(yp[i]);
    if (!string_equal(xi, yi)) {
      return ScalarLogical(0);
    }
  }
  return ScalarLogical(1);
}



//' @name where_square_bracket_opens
//' @param x Character vector of characters.
//' @param i position of closing bracket.
//'
//' @return
//' -1 if x[i] does not closing bracket
//'  0 if bracket never closes
//'  j the location of the closing brace
//'
//' @noRd


SEXP Cwhere_square_bracket_opens(SEXP xx, SEXP ii) {
  if (TYPEOF(xx) != STRSXP || xlength(xx) >= INT_MAX) {
    return R_NilValue; // # nocov
  }
  R_xlen_t N = xlength(xx);
  int i = asInteger(ii);
  if (i < 0 || i >= N) {
    return ScalarInteger(-1);
  }
  char xi = CHAR(STRING_ELT(xx, i))[0];
  if (xi != STOP_SQBRK) {
    return ScalarInteger(-1);
  }
  int depth = 0;
  for (int k = i; k >= 0; --k) {
    char xk = CHAR(STRING_ELT(xx, k))[0];
    depth += (xk == STOP_SQBRK) - (xk == OPEN_SQBRK);
    if (depth == 0) {
      return ScalarInteger(k);
    }
  }
  return ScalarInteger(0);
}

SEXP CextractMandatory(SEXP x, SEXP command, SEXP NCommands) {
  if (TYPEOF(x) != STRSXP || xlength(x) >= INT_MAX) {
    return R_NilValue; // # nocov
  }
  if (TYPEOF(NCommands) != INTSXP) {
    error("TYPEOF(NCommands) != INTSXP."); // # nocov
  }
  int nCommands = asInteger(NCommands);
  R_xlen_t N = xlength(x);
  unsigned int command_len = xlength(command);
  // const char *xp = x[0];
  // const char *cp = command[0];

  bool finish_extract = false;
  bool within_brace = false;
  // bool within_optional = false;
  int opt_group = 0;
  int cj = 0;

  int command_no = 0;

  int n_protect = 0;
  SEXP support = PROTECT(allocVector(STRSXP, N));
  ++n_protect;
  SEXP CommandNo = PROTECT(IntegerN(nCommands));
  ++n_protect;
  int * commandNo = INTEGER(CommandNo);
  SEXP CommandOpeners = PROTECT(IntegerN(nCommands));
  ++n_protect;
  int * commandOpeners = INTEGER(CommandOpeners);
  SEXP CommandClosers = PROTECT(IntegerN(nCommands));
  ++n_protect;
  int * commandClosers = INTEGER(CommandClosers);
  const char * command0 = CHAR(STRING_ELT(command, 0));

  char xk = '\0';
  for (R_xlen_t i = 0; i < N; ++i) {
    int k = 0;
    if (length(STRING_ELT(x, i)) == 0) {
      continue;
    }
    const char * xi = CHAR(STRING_ELT(x, i));
    if (string_equal(xi, command0)) {
      for (int ci = 1; ci < command_len; ++ci) {
        // Consider command = \abc and document ends with ab
        if (i + ci >= N) {
          break;
        }
        const char * cii = CHAR(STRING_ELT(command, ci));
        const char * xic = CHAR(STRING_ELT(x, i + ci));
        // if (x[i + ci] == command[ci]) {

        if (string_equal(xic, cii)) {
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
          ++k;
          xk = CHAR(STRING_ELT(x, k))[0];
          if (xk == ' ' || xk == '\0') {
            // \abc {def}
            continue;
          }
          if (xk == '[') {
            ++opt_group;
            int rel_opt_group = 1;
            while (rel_opt_group && k < N - 1) {
              // just keep moving forward until we get out of the current
              // optional group.
              ++k;
              if (length(STRING_ELT(x, k)) != 1) {
                continue; // # nocov
              }
              xk = CHAR(STRING_ELT(x, k))[0];
              if (xk == ']') {
                --rel_opt_group;
                --opt_group;
              } else {
                if (xk == '{') {
                  ++rel_group;
                  while (rel_group && k < N - 1) {
                    ++k;
                    if (length(STRING_ELT(x, k)) != 1) {
                      continue; // # nocov
                    }
                    xk = CHAR(STRING_ELT(x, k))[0];
                    if (xk == '}') {
                      --rel_group;
                    } else {
                      if (xk == '{') {
                        ++rel_group;
                      }
                    }
                  }
                }
                if (xk == '[') {
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
          xk = (length(STRING_ELT(x, k)) != 1) ? '\0' : CHAR(STRING_ELT(x, k))[0];
          within_brace = (rel_group == 0) && xk == '{';

          // abc{xyz} but not abcd{xyz}
          if (xk != '\0' && xk != ' ' && xk != '{') {
            k = N + 1;  // effectively break both while loops
          } else {
            commandOpeners[command_no] = k + 1;
          }
        }
        while (k < N) {
          SET_STRING_ELT(support, k, STRING_ELT(x, k));

          // #nocov start
          if (command_no >= nCommands) {
            UNPROTECT(n_protect);
            error("command_no overflow, k = %d, command_no = %d, nCommands = %d", k, command_no, nCommands);
          }
          // #nocov end
          // R indexing
          commandNo[command_no] = command_no + 1;
          xk = length(STRING_ELT(x, k)) != 1 ? '\0' : CHAR(STRING_ELT(x, k))[0];
          if (xk == '{') {
            ++rel_group;
          } else {
            if (xk == '}') {
              --rel_group;
            }
          }


          finish_extract = (rel_group == 0) && xk == '}';
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

  SEXP out = PROTECT(allocVector(VECSXP, 3));
  ++n_protect;
  SET_VECTOR_ELT(out, 0, support);
  SET_VECTOR_ELT(out, 1, CommandOpeners);
  SET_VECTOR_ELT(out, 2, CommandClosers);
  UNPROTECT(n_protect);
  return out;
}

// # nocov start
SEXP CPrintChars(SEXP x) {
  if (TYPEOF(x) != STRSXP) {
    return R_NilValue;
  }
  R_xlen_t N = xlength(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    const char * xi = CHAR(STRING_ELT(x, i));
    int j = 0;
    while (xi[j] != '\0') {
      Rprintf("%c", xi[j]);
      ++j;
    }
  }
  return R_NilValue;
}
// # nocov end




