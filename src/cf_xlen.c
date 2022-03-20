#include "hutilscpp.h"

int cf_xlen(SEXP x, SEXP y) {
  R_xlen_t Ny = xlength(y);
  if (Ny == 1) {
    return CF_LEN_1;
  }
  if (Ny == 2) {
    return CF_LEN_2;
  }
  if (Ny == xlength(x)) {
    return CF_LEN_N;
  }
  return 0;

}

int op_xlen2(int o) {
  return o == OP_BW || o == OP_BC || o == OP_BO || o == OP_WB;
}
