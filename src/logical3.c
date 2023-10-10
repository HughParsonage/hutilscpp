#include "hutilscpp.h"


SEXP Cor3(SEXP xx, SEXP yy, SEXP zz) {
  if (TYPEOF(xx) != LGLSXP ||
      TYPEOF(yy) != LGLSXP ||
      TYPEOF(zz) != LGLSXP) {
    return R_NilValue; // # nocov
  }
  R_xlen_t N = xlength(xx);
  if (xlength(yy) != N) {
    error("y and x have different lengths.");
  }
  if (xlength(zz) > 1 && xlength(zz) != N) {
    error("z has the wrong length");
  }
  const int * x = LOGICAL(xx);
  const int * y = LOGICAL(yy);
  const int * z = LOGICAL(zz);
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * out = LOGICAL(ans);
  if (xlength(zz) != N) {
    if (xlength(zz) == 0) {
      for (R_xlen_t i = 0; i < N; ++i) {
        out[i] = x[i] || y[i];
      }
    } else {
     if (z[0]) {
       for (R_xlen_t i = 0; i < N; ++i) {
         out[i] = true;
       }
     } else {
       for (R_xlen_t i = 0; i < N; ++i) {
         out[i] = x[i] || y[i];
       }
     }

    }
  } else {
    for (R_xlen_t i = 0; i < N; ++i) {
      out[i] = x[i] || y[i] || z[i];
    }
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cand3(SEXP xx, SEXP yy, SEXP zz) {
  if (TYPEOF(xx) != LGLSXP ||
      TYPEOF(yy) != LGLSXP ||
      TYPEOF(zz) != LGLSXP) {
    return R_NilValue; // # nocov
  }
  R_xlen_t N = xlength(xx);
  if (xlength(yy) != N) {
    error("y and x have different lengths.");
  }
  if (xlength(zz) > 1 && xlength(zz) != N) {
    error("z has the wrong length");
  }
  const int * x = LOGICAL(xx);
  const int * y = LOGICAL(yy);
  const int * z = LOGICAL(zz);
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * out = LOGICAL(ans);
  if (xlength(zz) != N) {
    if (xlength(zz) == 0) {
      for (R_xlen_t i = 0; i < N; ++i) {
        out[i] = x[i] && y[i];
      }
    } else {
      if (!z[0]) {
        for (R_xlen_t i = 0; i < N; ++i) {
          out[i] = false;
        }
      } else {
        for (R_xlen_t i = 0; i < N; ++i) {
          out[i] = x[i] && y[i];
        }
      }

    }
  } else {
    for (R_xlen_t i = 0; i < N; ++i) {
      out[i] = x[i] && y[i] && z[i];
    }
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cna_and(SEXP xx) {
  // NA & x
  R_xlen_t N = xlength(xx);
  const int * x = LOGICAL(xx);
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = (x[i] != FALSE) ? NA_LOGICAL : 0;
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cwhich3(SEXP xx, SEXP yy, SEXP zz,
             SEXP AAnd,
             SEXP AanyNAx,
             SEXP AanyNAy,
             SEXP AanyNAz) {
  R_xlen_t n = (xlength(xx) > 1) ? xlength(xx) : ((xlength(yy) > 1) ? xlength(yy) : xlength(zz));
  if (n >= INT_MAX) {
    // # not suitable for integer
    return R_NilValue; // # nocov
  }
  const bool nx = xlength(xx) == n;
  const bool ny = xlength(yy) == n;
  const bool nz = xlength(zz) == n;

  const int * x = LOGICAL(xx);
  const int * y = LOGICAL(yy);
  const int * z = LOGICAL(zz);

  const bool And = asLogical(AAnd);
  const bool anyNAx = asLogical(AanyNAx);
  const bool anyNAy = asLogical(AanyNAy);
  const bool anyNAz = asLogical(AanyNAz);

  SEXP ans = PROTECT(allocVector(INTSXP, n));
  int * out = INTEGER(ans);
  int j = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    int xi = nx ? x[i] : x[0];
    int yi = ny ? y[i] : y[0];
    int zi = nz ? z[i] : z[0];

    bool do_i = false;


    if (And) {
      if ((anyNAx ? ((xi != NA_LOGICAL) && xi) : xi) &&
          (anyNAy ? ((yi != NA_LOGICAL) && yi) : yi) &&
          (anyNAz ? ((zi != NA_LOGICAL) && zi) : zi)) {
        do_i = true;
      }
    } else {
      if ((anyNAx ? ((xi != NA_LOGICAL) && xi) : xi) ||
          (anyNAy ? ((yi != NA_LOGICAL) && yi) : yi) ||
          (anyNAz ? ((zi != NA_LOGICAL) && zi) : zi)) {
        do_i = true;
      }
    }
    if (do_i) {
      out[j] = i + 1;
      ++j;
    }
  }

  SEXP Ans = PROTECT(allocVector(VECSXP, 2));

  // rchk complains unless this is protected explicitly
  SEXP SIJ = PROTECT(ScalarInteger(j));
  SET_VECTOR_ELT(Ans, 0, SIJ);
  SET_VECTOR_ELT(Ans, 1, ans);
  UNPROTECT(3);
  return Ans;
}


SEXP Cwhich3_mem(SEXP xx, SEXP yy, SEXP zz, SEXP AAnd) {
  const bool And = asLogical(AAnd);
  R_xlen_t n = (xlength(xx) > 1) ? xlength(xx) : ((xlength(yy) > 1) ? xlength(yy) : xlength(zz));
  const bool nx = xlength(xx) == n;
  const bool ny = xlength(yy) == n;
  const bool nz = xlength(zz) == n;

  const int * x = LOGICAL(xx);
  const int * y = LOGICAL(yy);
  const int * z = LOGICAL(zz);

  R_xlen_t Count = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    int xi = nx ? x[i] : x[0];
    int yi = ny ? y[i] : y[0];
    int zi = nz ? z[i] : z[0];
    if (And) {
      if (xi && yi && zi) {
        ++Count;
      }
    } else {
      if (xi || yi || zi) {
        ++Count;
      }
    }
  }
  SEXP ans = PROTECT(allocVector(INTSXP, Count));
  int * restrict out = INTEGER(ans);

  R_xlen_t j = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    int xi = nx ? x[i] : x[0];
    int yi = ny ? y[i] : y[0];
    int zi = nz ? z[i] : z[0];
    if (And) {
      if (xi && yi && zi) {
        out[j] = i + 1;
        ++j;
      }
    } else {
      if (xi || yi || zi) {
        out[j] = i + 1;
        ++j;
      }
    }
  }
  UNPROTECT(1);
  return ans;
}


