#include "hutilscpp.h"

int do_op2M(const char * x) {
  char x00 = x[0];
  if (x00 == '\0') {
    return 0;
  }
  int nchar = x[1] == '\0' ? 1 : 2;
  char x01 = (nchar >= 2) ? x[1] : x00;
  switch(x00) {
  case '!':
    return OP_NE;
    break;
  case '=':
    return OP_EQ;
    break;
  case '>':
    return (nchar == 1) ? OP_GT : OP_GE;
    break;
  case '<':
    return (nchar == 1) ? OP_LT : OP_LE;
    break;
  case '%':
    switch (x01) {
    case 'i':
      return OP_IN;
      break;
    case 'b':
      return OP_BW;
      break;
    case '(':
      return OP_BO;
      break;
    case ']':
      return OP_BC;
      break;
    }
  }
  return 0;
}

SEXP C_op2M(SEXP xx) {
  if (TYPEOF(xx) != STRSXP ||
      xlength(xx) == 0 ||
      STRING_ELT(xx, 0) == NA_STRING) {
    return ScalarInteger(0); // # nocov
  }
  const char * x = CHAR(STRING_ELT(xx, 0));
  return ScalarInteger(do_op2M(x));
}
