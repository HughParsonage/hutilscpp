#include "hutilscpp.h"

SEXP ScalarLength(R_xlen_t o) {
  return (o < INT_MAX) ? ScalarInteger(o) : ScalarReal(o);
}
