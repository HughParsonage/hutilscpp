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
    case 'n':
      return OP_NI;
      break;
    }
  }
  return 0;
}

// # nocov start
// if  a op b  what is  b op a?
int rev_op(int op) {
  switch(op) {
  case OP_NE:
  case OP_EQ:
    return op;
  case OP_LT:
    return OP_GT;
  case OP_LE:
    return OP_GE;
  case OP_GT:
    return OP_LT;
  case OP_GE:
    return OP_LE;
  default:
    return 0;
  }
  return 0;
}

// if  a op b  what is !(a op b)?
int inv_op(int op) {
  switch(op) {
  case OP_NE:
    return OP_EQ;
  case OP_NI:
    return OP_IN;
  case OP_EQ:
    return OP_NE;
  case OP_IN:
    return OP_NI;
  case OP_GE:
    return OP_LT;
  case OP_LT:
    return OP_GE;
  case OP_LE:
    return OP_GT;
  case OP_GT:
    return OP_LE;
  case OP_BW:
    return OP_WB;
  case OP_WB:
    return OP_BW;
  case OP_BO:
    return OP_BC;
  case OP_BC:
    return OP_BO;
  default:
    return 0;
  }
  return 0;
}



int sex2op(SEXP oo) {
  switch(TYPEOF(oo)) {
  case STRSXP:
    return do_op2M(CHAR(STRING_ELT(oo, 0)));
  case INTSXP:
    return asInteger(oo);
  }
  return 0;
}
// # nocov end

SEXP C_op2M(SEXP xx) {
  if (TYPEOF(xx) != STRSXP ||
      xlength(xx) == 0 ||
      STRING_ELT(xx, 0) == NA_STRING) {
    return ScalarInteger(0); // # nocov
  }
  const char * x = CHAR(STRING_ELT(xx, 0));
  return ScalarInteger(do_op2M(x));
}
