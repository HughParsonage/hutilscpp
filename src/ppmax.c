#include "hutilsc.h"
#include <Rmath.h>

SEXP do_c_pmax(SEXP x, SEXP a, SEXP b) {
  R_len_t nx = length(x), na = length(a), nb = length(b);
  if (!isReal(x) || !isReal(a) || !isReal(b)) {
    error("x is not a real.");
  }
  if (na != 1) {
    error("a did not have length 1");
  }
  if (nb != 1) {
    error("b did not have length 1");
  }

  SEXP ans = PROTECT(allocVector(REALSXP, nx));
  double *restrict ansp = REAL(ans);
  const double *ap = REAL(a);
  const double *bp = REAL(b);
  const double *xp = REAL(x);
  const double aaa = ap[0];
  const double bbb = bp[0];

  if (aaa != R_PosInf && bbb != R_NegInf) {
    for (int i = 0; i < nx; i++) {
      double eleme = xp[i];
      ansp[i] = eleme;
      if (eleme >= aaa && eleme <= bbb) {
        continue;
      }
      ansp[i] = eleme < aaa ? aaa : bbb;
    }
  } else {
    if (aaa != R_PosInf) {
      for (int i = 0; i < nx; i++) {
        double eleme = xp[i];
        ansp[i] = eleme > aaa ? eleme : aaa;
      }
    } else {
      for (int i = 0; i < nx; i++) {
        double eleme = xp[i];
        ansp[i] = eleme < bbb ? eleme : bbb;
      }
    }
  }
  UNPROTECT(1);
  return(ans);
}

