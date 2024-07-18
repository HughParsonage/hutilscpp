#include "hutilscpp.h"

void assertInteger(SEXP x, const char * var) {
  if (!isInteger(x)) {
    error("`%s` was type '%s' but must be type integer.", var, type2char(TYPEOF(x)));
  }
}
