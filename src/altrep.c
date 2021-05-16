#include "hutilscpp.h"

SEXP Cis_altrep(SEXP x) {
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 5, 0)
  return ScalarLogical(ALTREP(x));
#else
  return ScalarLogical(0);
#endif
}
