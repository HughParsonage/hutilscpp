#include "hutilscpp.h"

int asInteger2(SEXP x) {
  if (xlength(x) >= 1) {
    switch(TYPEOF(x)) {
    case INTSXP:
      return INTEGER_ELT(x, 0);
    case REALSXP:
      return dbl2int(REAL_ELT(x, 0));
    }
  }
  return NA_INTEGER; // # nocov
}
