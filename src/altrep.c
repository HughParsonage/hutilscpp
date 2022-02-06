#include "hutilscpp.h"

bool is_altrep(SEXP x) {
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 5, 0)
  return ALTREP(x);
#else
  return 0;
#endif
}

SEXP Cis_altrep(SEXP x) {
  return ScalarLogical(is_altrep(x));
}
