#include "hutilscpp.h"

bool isntRaw(SEXP x) {
  return TYPEOF(x) != RAWSXP;
}
