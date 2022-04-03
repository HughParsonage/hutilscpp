#include "hutilscpp.h"

bool isingle_ox_x1_x2(int x, int oix, int x1, int x2) {
  switch(oix) {
  case OP_NE:
    return x != x1;

  case OP_EQ:
    return x == x1;

  case OP_GE:
    return x >= x1;

  case OP_LE:
    return x <= x1;

  case OP_GT:
    return x >  x1;

  case OP_LT:
    return x <  x1;

  case OP_BW:
    return x >= x1 && x <= x2;

  case OP_BO:
    return x >  x1 && x <  x2;

  case OP_BC:
    return x <= x1 || x >= x2;

    // # nocov start
  case OP_IN:
    return x == x1 || x == x2;

  case OP_NI:
    return x != x1 && x != x2;
    // # nocov end
  }
  return false; // # nocov
}

bool dsingle_ox_x1_x2(double x, int oix, double x1, double x2) {
  switch(oix) {
  case OP_NE:
    return x != x1;

  case OP_EQ:
    return x == x1;

  case OP_GE:
    return x >= x1;

  case OP_LE:
    return x <= x1;

  case OP_GT:
    return x >  x1;

  case OP_LT:
    return x <  x1;

  case OP_BW:
    return x >= x1 && x <= x2;

  case OP_BO:
    return x >  x1 && x <  x2;

  case OP_BC:
    return x <= x1 || x >= x2;

    // # nocov start
  case OP_IN:
    return x == x1 || x == x2;

  case OP_NI:
    return x != x1 && x != x2;
    // # nocov end
  }
  return false; // # nocov
}
