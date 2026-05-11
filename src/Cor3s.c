#include "hutilscpp.h"

// Phase 3 (#44): the predicate kernels live in src/vops_kernels.h, which is
// included once with VOPS_OR (this file -> vor2s_*), once with VOPS_AND
// (Cand3s.c -> vand2s_*), and once with VOPS_INIT in each .c file (the
// first-predicate direct-write fast path -- see Phase 2.5).

#define VOPS_OR
#include "vops_kernels.h"

#define VOPS_INIT
#include "vops_kernels.h"

SEXP Cors(SEXP oo1, SEXP xx1, SEXP yy1,
          SEXP oo2, SEXP xx2, SEXP yy2,
          SEXP nthreads) {
  R_xlen_t N = xlength(xx1);
  const bool use2 = oo2 != R_NilValue;
  if (use2 && xlength(xx2) != N) {
    // # nocov start
    error("`(Cors): xlength(xx1) = %lld`, yet `xlength(xx2) = %lld`.",
          (long long)xlength(xx1), (long long)xlength(xx2));
    // # nocov end
  }

  int nThread = as_nThread(nthreads);
  const int o1 = sex2op(oo1);
  const int o2 = sex2op(oo2);
  if (o1 == 0 || (use2 && o2 == 0)) {
    return R_NilValue;
  }
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * ansp = RAW(ans);
  int err[1] = {0};

  // Phase 2.5: see Cand3s.c -- the first predicate writes directly via
  // INIT-mode dispatch (or via the inline NIL handler below), the
  // second predicate combines with `|=`. No unconditional memset.

  if (yy1 == R_NilValue) {
    switch (TYPEOF(xx1)) {
    case LGLSXP: {
      const int * xx1p = LOGICAL(xx1);
      if (o1 == OP_NE) {
        FORLOOP(ansp[i] = xx1p[i] != 1;)
      } else {
        FORLOOP(ansp[i] = xx1p[i] != 0;)
      }
    }
      break;
    case RAWSXP: {
      // See Cand3s.c entry: OP_NE on a raw mask must use `byte == 0`
      // (boolean falsy) so that truthy non-{0,1} bytes (e.g. an external
      // mask containing 2) are correctly excluded; otherwise this entry
      // disagrees with the dispatcher's KFN(R) and `or3s(!m)` differs
      // from `or3s(FALSE-vec, !m)`.
      const unsigned char * xx1p = RAW(xx1);
      if (o1 == OP_NE) {
        FORLOOP(ansp[i] = xx1p[i] == 0;)
      } else {
        FORLOOP(ansp[i] = xx1p[i] != 0;)
      }
    }
      break;
      // # nocov start
    default:
      // No first-predicate state established; safest to mark unsupported
      // and let the wrapper fall back rather than leave the mask
      // uninitialised.
      memset(ansp, 0, N);
      err[0] = OR3__UNSUPPORTED_TYPEX;
      // # nocov end
    }
  } else {
    vinit2s_dispatch(ansp, o1, xx1, yy1, nThread, err);
  }
  if (use2) {
    vor2s_dispatch(ansp, o2, xx2, yy2, nThread, err);
  }
  UNPROTECT(1);
  if (err[0]) {
    return R_NilValue;
  }
  return ans;
}
