#include "hutilscpp.h"

bool isntRaw(SEXP x) {
  return TYPEOF(x) != RAWSXP;
}

bool isRaw(SEXP x) {
  return TYPEOF(x) == RAWSXP;
}
