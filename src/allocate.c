#include "hutilsc.h"

SEXP Callocate0_int(SEXP N, SEXP nThread) {
  if (xlength(N) != 1 || (TYPEOF(N) != INTSXP && TYPEOF(N) != REALSXP)) {
    error("N not a single number.");
  }
  if (xlength(nThread) != 1 ||
      (TYPEOF(nThread) != INTSXP && TYPEOF(nThread) != REALSXP)) {
    error("nThread not a single number.");
  }
  R_xlen_t n = TYPEOF(N) == INTSXP ? asInteger(N) : asReal(N);
  int nthreads = asInteger(nThread);
  SEXP ans = PROTECT(allocVector(INTSXP, n));

  int * ansp = INTEGER(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nthreads)
#endif
  for (R_xlen_t i = 0; i < n; ++i) {
    ansp[i] = 0;
  }
  UNPROTECT(1);
  return ans;
}

SEXP Callocate0_dbl(SEXP N, SEXP nThread) {
  if (xlength(N) != 1 || (TYPEOF(N) != INTSXP && TYPEOF(N) != REALSXP)) {
    error("N not a single number.");
  }
  if (xlength(nThread) != 1 ||
      (TYPEOF(nThread) != INTSXP && TYPEOF(nThread) != REALSXP)) {
    error("nThread not a single number.");
  }
  R_xlen_t n = TYPEOF(N) == INTSXP ? asInteger(N) : asReal(N);
  int nthreads = asInteger(nThread);
  SEXP ans = PROTECT(allocVector(REALSXP, n));

  double * ansp = REAL(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nthreads)
#endif
  for (R_xlen_t i = 0; i < n; ++i) {
    ansp[i] = 0;
  }
  UNPROTECT(1);
  return ans;
}

SEXP Sallocate_with_root(R_xlen_t N,
                         int a,
                         R_xlen_t r,
                         bool left,
                         bool do_pmin,
                         int nThread) {
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * out = INTEGER(ans);

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    bool post = i >= r;
    if (post != left) {
      out[i] = a;
      continue;
    }
    if (do_pmin) {
      out[i] = post ? (r - i) : (i - r);
    } else {
      out[i] = post ? (i - r) : (r - i);
    }
    out[i] += a;
  }
  UNPROTECT(1);
  return ans;
}

SEXP Callocate_with_root(SEXP NN, SEXP aa, SEXP rr, SEXP lleft, SEXP ddo_pmin, SEXP nthreads) {
  R_xlen_t N = isReal(NN) ? asReal(NN) : asInteger(NN);
  int a = asInteger(aa);
  R_xlen_t r = isReal(rr) ? asReal(rr) : asInteger(rr);
  const bool left = asLogical(lleft);
  const bool do_pmin = asLogical(ddo_pmin);
  int nThread = asInteger(nthreads);
  return Sallocate_with_root(N, a, r, left, do_pmin, nThread);
}

SEXP Callocate0_except(SEXP NN, SEXP Ind, SEXP Vic, SEXP nthread) {
  R_xlen_t N = isReal(NN) ? asReal(NN) : asInteger(NN);
  int nThread = asInteger(nthread);
  if (TYPEOF(Ind) != INTSXP || TYPEOF(Vic) != INTSXP) {
    error("Internal error(Callocate0_except): wrong types"); // # nocov
  }
  R_xlen_t tn = xlength(Ind);
  R_xlen_t vn = xlength(Vic);

  const int * India = INTEGER(Ind);
  const int * Victor = INTEGER(Vic);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * ansp = INTEGER(ans);

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = 0;
  }
  if (vn < 1 || tn < 1) {
    warning("Ignoring Victor."); // # nocov
    UNPROTECT(1); // # nocov
    return ans; // # nocov
  }
  for (R_xlen_t j = 0; j < tn; ++j) {
    R_xlen_t i = India[j];
    if (i < 0 || i >= N) {
      continue; // # nocov
    }
    int v = (tn == vn) ? Victor[j] : Victor[0];
    ansp[i] = v;
  }
  UNPROTECT(1);
  return ans;


}
