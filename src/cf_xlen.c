#include "hutilscpp.h"

int op_xlen2(int o) {
  return o == OP_BW || o == OP_BC || o == OP_BO || o == OP_WB;
}
