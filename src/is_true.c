#include "hutilscpp.h"

bool is_true(SEXP x) {
  return (TYPEOF(x) == LGLSXP) && (xlength(x) == 1) && (LOGICAL_ELT(x, 0) == 1);
}
