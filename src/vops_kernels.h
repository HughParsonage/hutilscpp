// Phase-3 shared dispatch kernels for `and3s` / `or3s`.
//
// Parameterised on one of three macros, controlling how the predicate
// result combines with the running mask:
//
//   VOPS_AND   ansp[i] &= (e)    accumulate AND with prior mask
//   VOPS_OR    ansp[i] |= (e)    accumulate OR  with prior mask
//   VOPS_INIT  ansp[i]  = (e)    direct write -- no prior mask read
//
// `VOPS_INIT` (Phase 2.5) lets the entry function skip the initial
// `memset(ansp, 1/0, N)` for the FIRST predicate by handing the kernel
// a "no prior state, write directly" mode. This recovers the pre-
// Phase-2 memory-traffic profile (one `write` per first predicate)
// without giving up Phase 2's mask-init-once invariant for predicates
// 2+: those still go through `&=` / `|=`. The same .c file includes
// this header twice -- once for AND/OR, once for INIT -- so every
// translation unit ends up with both `vand2s_*` (or `vor2s_*`) and
// `vinit2s_*` static kernels.
//
// All macros defined here are `#undef`ed at the end of the file so
// the multi-include pattern works.

#if !defined(VOPS_AND) && !defined(VOPS_OR) && !defined(VOPS_INIT)
#error "vops_kernels.h requires VOPS_AND, VOPS_OR, or VOPS_INIT"
#endif

#ifdef VOPS_AND
#define KFN_(name) vand2s_##name
#define MASK_COMBINE(i, expr) (ansp[(i)] &= (expr))
#define ALWAYS_TRUE_PRED() do { return; } while (0)
#define ALWAYS_FALSE_PRED() do { memset(ansp, 0, N); return; } while (0)
#define UNRESOLVED(i) (ansp[(i)] != 0)
#define ANNIHILATOR_VAL 0
#define UNSUPPORTED_TYPEX AND3_UNSUPPORTED_TYPEX
#define UNSUPPORTED_TYPEY AND3_UNSUPPORTED_TYPEY
#define ORAND_TAG ORAND_AND
#endif

#ifdef VOPS_OR
#define KFN_(name) vor2s_##name
#define MASK_COMBINE(i, expr) (ansp[(i)] |= (expr))
#define ALWAYS_TRUE_PRED() do { memset(ansp, 1, N); return; } while (0)
#define ALWAYS_FALSE_PRED() do { return; } while (0)
#define UNRESOLVED(i) (ansp[(i)] == 0)
#define ANNIHILATOR_VAL 1
#define UNSUPPORTED_TYPEX OR3__UNSUPPORTED_TYPEX
#define UNSUPPORTED_TYPEY OR3__UNSUPPORTED_TYPEY
#define ORAND_TAG ORAND_OR
#endif

#ifdef VOPS_INIT
// INIT mode: no prior mask state. The kernel writes directly. The
// "always-{true,false} predicate" early returns memset to that value
// because there is no identity to default to. UNRESOLVED is always
// TRUE because every row is fresh -- this disables the OP_NI / OP_IN
// short-circuits that combine mode uses; the kernel still produces
// the right answer, just without skipping pre-resolved rows. The
// shared err-codes piggyback on the AND constants since INIT only
// runs as the first predicate of an AND or OR call.
#define KFN_(name) vinit2s_##name
#define MASK_COMBINE(i, expr) (ansp[(i)] = (unsigned char)(expr))
#define ALWAYS_TRUE_PRED() do { memset(ansp, 1, N); return; } while (0)
#define ALWAYS_FALSE_PRED() do { memset(ansp, 0, N); return; } while (0)
#define UNRESOLVED(i) 1
#define ANNIHILATOR_VAL 0
#define UNSUPPORTED_TYPEX AND3_UNSUPPORTED_TYPEX
#define UNSUPPORTED_TYPEY AND3_UNSUPPORTED_TYPEY
#define ORAND_TAG ORAND_EQ
#endif

#define KFN(name) KFN_(name)

#ifndef HUTILSCPP_VOPS_HELPERS
#define HUTILSCPP_VOPS_HELPERS
static inline bool vops_op_is_compare(const int o) {
  return o == OP_NE || o == OP_EQ ||
    o == OP_GE || o == OP_LE || o == OP_GT || o == OP_LT;
}

static inline bool vops_op_is_between(const int o) {
  return o == OP_BW || o == OP_BO || o == OP_BC;
}

static inline bool vops_op_is_unary_mask(const int o) {
  return o == OP_EQ || o == OP_NE;
}

static inline bool vops_nonraw_supported(const int o,
                                         const R_xlen_t M,
                                         const R_xlen_t N) {
  if (M == 2 && vops_op_is_between(o)) {
    return true;
  }
  return (M == 1 || M == N) && vops_op_is_compare(o);
}
#endif

#define FORLOOP_combine_op(op, rhs)                                 \
  FORLOOP({ MASK_COMBINE(i, x[i] op (rhs)); })

static void KFN(II)(unsigned char * ansp, const int o,
                    const int * x, R_xlen_t N,
                    const int * y, R_xlen_t M,
                    int nThread) {
  if (M == 2 && op_xlen2(o)) {
    const bool y0_NA = y[0] == NA_INTEGER;
    const bool y1_NA = y[1] == NA_INTEGER;
    if (y0_NA || y1_NA) {
      // Mirror R/between.R: NA bound is an open bound. (NA, NA) -> all
      // TRUE, (NA, b) -> upper-half, (a, NA) -> lower-half. x-NA stays
      // FALSE here to match the kernel's na = "C" convention.
      if (y0_NA && y1_NA) ALWAYS_TRUE_PRED();
      switch (o) {
      case OP_BW:
        if (y0_NA) {
          FORLOOP({ int xi = x[i]; MASK_COMBINE(i, xi != NA_INTEGER && xi <= y[1]); });
          return;
        }
        FORLOOP({ int xi = x[i]; MASK_COMBINE(i, xi != NA_INTEGER && xi >= y[0]); });
        return;
      case OP_BO:
        if (y0_NA) {
          FORLOOP({ int xi = x[i]; MASK_COMBINE(i, xi != NA_INTEGER && xi <  y[1]); });
          return;
        }
        FORLOOP({ int xi = x[i]; MASK_COMBINE(i, xi != NA_INTEGER && xi >  y[0]); });
        return;
      case OP_BC:
        if (y0_NA) {
          FORLOOP({ int xi = x[i]; MASK_COMBINE(i, xi != NA_INTEGER && xi >= y[1]); });
          return;
        }
        FORLOOP({ int xi = x[i]; MASK_COMBINE(i, xi != NA_INTEGER && xi <= y[0]); });
        return;
      }
    }
    int y0 = y[0];
    int y1 = y[1];
    if (y0 > y1) {
      // [y0,y1], (y0,y1) all empty -> always-false; BC with inverted bounds
      // is the package-defined "complement of an inverted range" which is
      // also empty (matches `%]between[%` R-side semantics).
      ALWAYS_FALSE_PRED();
    }
    if (y0 == y1) {
      switch (o) {
      case OP_BW:
        FORLOOP_combine_op(==, y0);
        return;
      case OP_BO:
        ALWAYS_FALSE_PRED();
      case OP_BC:
        ALWAYS_TRUE_PRED();
      }
      return;
    }
    switch (o) {
    case OP_BW: {
      if (y0 == 0) {
        FORLOOP({
          unsigned int xi = x[i];
          MASK_COMBINE(i, xi <= (unsigned int)y1);
        });
      } else if (y0 > 0) {
        unsigned int u0 = y0;
        unsigned int u1 = (unsigned int)y1 - u0;
        FORLOOP({
          MASK_COMBINE(i, ((unsigned int)x[i] - u0) <= u1);
        });
      } else {
        FORLOOP({
          int xi = x[i];
          MASK_COMBINE(i, xi >= y0 && xi <= y1);
        });
      }
    }
      break;
    case OP_BO:
      // y0 < y1 here. For adjacent integer bounds (y1 - y0 == 1) the
      // open interval (y0, y1) contains no integers; betweeniiuu would
      // be called with a > b and wrap into an all-true range, so
      // short-circuit it.
      if (((unsigned int)y1 - (unsigned int)y0) < 2u) {
        ALWAYS_FALSE_PRED();
      }
      // + 1u guards y0 == INT_MAX
      FORLOOP({ MASK_COMBINE(i, betweeniiuu(x[i], (unsigned int)y0 + 1u, (unsigned int)y1 - 1u)); });
      break;
    case OP_BC:
      // Symmetric case: complement of the empty open interval is
      // everything.
      if (((unsigned int)y1 - (unsigned int)y0) < 2u) {
        ALWAYS_TRUE_PRED();
      }
      FORLOOP({ MASK_COMBINE(i, !betweeniiuu(x[i], (unsigned int)y0 + 1u, (unsigned int)y1 - 1u)); });
      break;
    }
    return;
  }
  if (M == N) {
    switch (o) {
    case OP_NE: FORLOOP_combine_op(!=, y[i]); break;
    case OP_EQ: FORLOOP_combine_op(==, y[i]); break;
    case OP_GT: FORLOOP_combine_op(>,  y[i]); break;
    case OP_LT: FORLOOP_combine_op(<,  y[i]); break;
    case OP_GE: FORLOOP_combine_op(>=, y[i]); break;
    case OP_LE: FORLOOP_combine_op(<=, y[i]); break;
    }
    return;
  }
  if (M == 1) {
    int y0 = y[0];
    switch (o) {
    case OP_NE: FORLOOP_combine_op(!=, y0); break;
    case OP_EQ: FORLOOP_combine_op(==, y0); break;
    case OP_GT: FORLOOP_combine_op(>,  y0); break;
    case OP_LT: FORLOOP_combine_op(<,  y0); break;
    case OP_GE: FORLOOP_combine_op(>=, y0); break;
    case OP_LE: FORLOOP_combine_op(<=, y0); break;
    }
  }
}

static void KFN(ID)(unsigned char * ansp, const int o,
                    const int * x, R_xlen_t N,
                    const double * y, R_xlen_t M,
                    int nThread) {
  if (M == 2 && op_xlen2(o)) {
    bool y0_NAN = ISNAN(y[0]);
    bool y1_NAN = ISNAN(y[1]);

    if (o == OP_BC) {
      if (y0_NAN && y1_NAN) {
        // R-level `%]between[%` returns rep(TRUE, length(x)) for c(NA, NA).
        // Pre-Phase-2.5 this returned silently, relying on the entry's
        // memset(1, N) for AND -- correct for AND by accident, wrong for
        // OR (mask stayed at 0), and uninitialised in INIT mode. Always
        // write the mask explicitly here.
        ALWAYS_TRUE_PRED();
      }
      if (y0_NAN) {
        FORLOOP({ MASK_COMBINE(i, x[i] >= y[1]); });
        return;
      }
      if (y1_NAN) {
        FORLOOP({ MASK_COMBINE(i, x[i] <= y[0]); });
        return;
      }
    }
    double pre_y0 = y0_NAN ? R_NegInf : y[0];
    double pre_y1 = y1_NAN ? R_PosInf : y[1];
    if (pre_y0 > pre_y1) {
      ALWAYS_FALSE_PRED();
    }
    switch (o) {
    case OP_BW:
      uc_betweenidd(ansp, ORAND_TAG, x, N, nThread, pre_y0, pre_y1);
      break;
    case OP_BO:
      FORLOOP({ MASK_COMBINE(i, (x[i] > pre_y0) && (x[i] < pre_y1)); });
      break;
    case OP_BC:
      FORLOOP({ MASK_COMBINE(i, (x[i] <= pre_y0) || (x[i] >= pre_y1)); });
      break;
    }
    return;
  }
  if (M == N) {
    switch (o) {
    case OP_NE: FORLOOP_combine_op(!=, y[i]); break;
    case OP_EQ: FORLOOP_combine_op(==, y[i]); break;
    case OP_GT: FORLOOP_combine_op(>,  y[i]); break;
    case OP_LT: FORLOOP_combine_op(<,  y[i]); break;
    case OP_GE: FORLOOP_combine_op(>=, y[i]); break;
    case OP_LE: FORLOOP_combine_op(<=, y[i]); break;
    }
    return;
  }
  if (M == 1) {
    double pre_y0 = y[0];
    int safety = why_dbl_isnt_int(pre_y0);
    int y0 = (safety == DBL_INT) ? (int)pre_y0 : 0;
    switch (o) {
    case OP_NE:
      // For non-integer / out-of-range / NaN scalar y, no integer x equals
      // y, so x != y is constant TRUE. Document NaN -> FALSE convention
      // already returns TRUE for !=.
      switch (safety) {
      case DBL_INT: FORLOOP_combine_op(!=, y0); return;
      case DBL_NAN:
      case DBL_FRA:
      case DBL_XHI:
      case DBL_XLO: ALWAYS_TRUE_PRED();
      }
      return;
    case OP_EQ:
      switch (safety) {
      case DBL_INT: FORLOOP_combine_op(==, y0); return;
      case DBL_NAN:
      case DBL_FRA:
      case DBL_XHI:
      case DBL_XLO: ALWAYS_FALSE_PRED();
      }
      return;
    case OP_GE:
    case OP_GT:
      // x op y_frac reduces to x >= floor(y_frac) + 1 for integer x:
      //   x >  y_frac  iff  x >  floor(y_frac)  iff  x >= floor(y_frac)+1
      //   x >= y_frac  iff  x >  floor(y_frac)  iff  x >= floor(y_frac)+1
      switch (safety) {
      case DBL_INT:
        if (o == OP_GE) { FORLOOP_combine_op(>=, y0); }
        else            { FORLOOP_combine_op(>,  y0); }
        return;
      case DBL_FRA: {
        int floor_y = (int)pre_y0 - (pre_y0 < 0);
        y0 = floor_y + 1;
        FORLOOP_combine_op(>=, y0);
      }
        return;
      case DBL_XHI: ALWAYS_FALSE_PRED();
      case DBL_XLO: ALWAYS_TRUE_PRED();
      case DBL_NAN: ALWAYS_FALSE_PRED();
      }
      return;
    case OP_LE:
    case OP_LT:
      //   x <  y_frac  iff  x <= floor(y_frac)
      //   x <= y_frac  iff  x <= floor(y_frac)
      switch (safety) {
      case DBL_INT:
        if (o == OP_LE) { FORLOOP_combine_op(<=, y0); }
        else            { FORLOOP_combine_op(<,  y0); }
        return;
      case DBL_FRA: {
        int floor_y = (int)pre_y0 - (pre_y0 < 0);
        y0 = floor_y;
        FORLOOP_combine_op(<=, y0);
      }
        return;
      case DBL_XHI: ALWAYS_TRUE_PRED();
      case DBL_XLO: ALWAYS_FALSE_PRED();
      case DBL_NAN: ALWAYS_FALSE_PRED();
      }
      return;
    }
  }
}

static void KFN(DI)(unsigned char * ansp, const int o,
                    const double * x, R_xlen_t N,
                    const int * y, R_xlen_t M,
                    int nThread) {
  if (M == 2 && op_xlen2(o)) {
    const bool y0_NA = y[0] == NA_INTEGER;
    const bool y1_NA = y[1] == NA_INTEGER;
    if (y0_NA || y1_NA) {
      // Open-bound semantics for NA bounds, matching R/between.R and the
      // ID / DD kernels. x is double so NaN comparisons are already
      // FALSE under IEEE 754 -- no separate x-NA guard needed.
      if (y0_NA && y1_NA) ALWAYS_TRUE_PRED();
      switch (o) {
      case OP_BW:
        if (y0_NA) { FORLOOP({ MASK_COMBINE(i, x[i] <= y[1]); }); return; }
        FORLOOP({ MASK_COMBINE(i, x[i] >= y[0]); });
        return;
      case OP_BO:
        if (y0_NA) { FORLOOP({ MASK_COMBINE(i, x[i] <  y[1]); }); return; }
        FORLOOP({ MASK_COMBINE(i, x[i] >  y[0]); });
        return;
      case OP_BC:
        if (y0_NA) { FORLOOP({ MASK_COMBINE(i, x[i] >= y[1]); }); return; }
        FORLOOP({ MASK_COMBINE(i, x[i] <= y[0]); });
        return;
      }
    }
    int y0 = y[0];
    int y1 = y[1];
    if (y0 > y1) {
      ALWAYS_FALSE_PRED();
    }
    switch (o) {
    case OP_BW:
      FORLOOP({ MASK_COMBINE(i, x[i] >= y0 && x[i] <= y1); });
      return;
    case OP_BO:
      FORLOOP({ MASK_COMBINE(i, x[i] > y0 && x[i] < y1); });
      return;
    case OP_BC:
      FORLOOP({ MASK_COMBINE(i, x[i] <= y0 || x[i] >= y1); });
      return;
    }
  }
  if (M == N) {
    switch (o) {
    case OP_NE: FORLOOP_combine_op(!=, y[i]); break;
    case OP_EQ: FORLOOP_combine_op(==, y[i]); break;
    case OP_GT: FORLOOP_combine_op(>,  y[i]); break;
    case OP_LT: FORLOOP_combine_op(<,  y[i]); break;
    case OP_GE: FORLOOP_combine_op(>=, y[i]); break;
    case OP_LE: FORLOOP_combine_op(<=, y[i]); break;
    }
    return;
  }
  if (M == 1) {
    int y0 = y[0];
    switch (o) {
    case OP_NE: FORLOOP_combine_op(!=, y0); break;
    case OP_EQ: FORLOOP_combine_op(==, y0); break;
    case OP_GT: FORLOOP_combine_op(>,  y0); break;
    case OP_LT: FORLOOP_combine_op(<,  y0); break;
    case OP_GE: FORLOOP_combine_op(>=, y0); break;
    case OP_LE: FORLOOP_combine_op(<=, y0); break;
    }
  }
}

static void KFN(DD)(unsigned char * ansp, const int o,
                    const double * x, R_xlen_t N,
                    const double * y, R_xlen_t M,
                    int nThread) {
  if (M == 2 && op_xlen2(o)) {
    // Mirror the ID kernel's NaN-bound handling. NaN comparisons in C
    // are all false, so a naive `x op NaN` FORLOOP would silently give
    // wrong answers (e.g. all-FALSE for `x %]between[% c(NA, NA)` where
    // R-level returns all-TRUE). Treat NaN as an open bound.
    bool y0_NAN = ISNAN(y[0]);
    bool y1_NAN = ISNAN(y[1]);
    if (o == OP_BC) {
      if (y0_NAN && y1_NAN) ALWAYS_TRUE_PRED();
      if (y0_NAN) {
        FORLOOP({ MASK_COMBINE(i, x[i] >= y[1]); });
        return;
      }
      if (y1_NAN) {
        FORLOOP({ MASK_COMBINE(i, x[i] <= y[0]); });
        return;
      }
    }
    double pre_y0 = y0_NAN ? R_NegInf : y[0];
    double pre_y1 = y1_NAN ? R_PosInf : y[1];
    if (pre_y0 > pre_y1) {
      ALWAYS_FALSE_PRED();
    }
    switch (o) {
    case OP_BW:
      FORLOOP({ MASK_COMBINE(i, (x[i] >= pre_y0) && (x[i] <= pre_y1)); });
      return;
    case OP_BO:
      FORLOOP({ MASK_COMBINE(i, (x[i] > pre_y0) && (x[i] < pre_y1)); });
      return;
    case OP_BC:
      FORLOOP({ MASK_COMBINE(i, (x[i] <= pre_y0) || (x[i] >= pre_y1)); });
      return;
    }
  }
  if (M == N) {
    switch (o) {
    case OP_NE: FORLOOP_combine_op(!=, y[i]); break;
    case OP_EQ: FORLOOP_combine_op(==, y[i]); break;
    case OP_GT: FORLOOP_combine_op(>,  y[i]); break;
    case OP_LT: FORLOOP_combine_op(<,  y[i]); break;
    case OP_GE: FORLOOP_combine_op(>=, y[i]); break;
    case OP_LE: FORLOOP_combine_op(<=, y[i]); break;
    }
    return;
  }
  if (M == 1) {
    double y0 = y[0];
    switch (o) {
    case OP_NE: FORLOOP_combine_op(!=, y0); break;
    case OP_EQ: FORLOOP_combine_op(==, y0); break;
    case OP_GT: FORLOOP_combine_op(>,  y0); break;
    case OP_LT: FORLOOP_combine_op(<,  y0); break;
    case OP_GE: FORLOOP_combine_op(>=, y0); break;
    case OP_LE: FORLOOP_combine_op(<=, y0); break;
    }
  }
}

static void KFN(LL)(unsigned char * ansp, const int o,
                    const int * x, R_xlen_t N,
                    const int * y, R_xlen_t M,
                    int nThread) {
  if (M == 1) {
    const int y0 = y[0];
    switch (o) {
    case OP_NI:
    case OP_NE: FORLOOP({ MASK_COMBINE(i, x[i] != y0); }); break;
    case OP_IN:
    case OP_EQ: FORLOOP({ MASK_COMBINE(i, x[i] == y0); }); break;
    case OP_GE: FORLOOP({ MASK_COMBINE(i, x[i] >= y0); }); break;
    case OP_LE: FORLOOP({ MASK_COMBINE(i, x[i] <= y0); }); break;
    case OP_GT: FORLOOP({ MASK_COMBINE(i, x[i] >  y0); }); break;
    case OP_LT: FORLOOP({ MASK_COMBINE(i, x[i] <  y0); }); break;
    }
    return;
  }
  if (M == 2) {
    switch (o) {
    case OP_BW: {
      // Logical x in {0,1}. Treat NA bounds as open (matches the
      // numeric ID kernel's NaN handling): NA on the lower bound means
      // unbounded below, NA on the upper bound means unbounded above.
      // Without this normalisation `x <= NA_LOGICAL` (== INT_MIN)
      // is false for every logical x, so e.g. `x %between% c(F, NA)`
      // returned all FALSE instead of all TRUE.
      int yy0 = (y[0] == NA_LOGICAL) ? INT_MIN : y[0];
      int yy1 = (y[1] == NA_LOGICAL) ? INT_MAX : y[1];
      if (yy0 == 0 && yy1 == 1) {
        ALWAYS_TRUE_PRED();
      } else {
        FORLOOP({ MASK_COMBINE(i, x[i] >= yy0 && x[i] <= yy1); });
        return;
      }
    }
      // # nocov start
    case OP_WB:
      if (y[0] == 0 && y[1] == 1) {
        ALWAYS_FALSE_PRED();
      } else {
        if (y[0] == 1) {
          if (y[1] == 0) {
            // x in {1} or x in {0} -> always-true predicate
            ALWAYS_TRUE_PRED();
          }
          FORLOOP({ MASK_COMBINE(i, x[i] == 1); });
          return;
        }
        if (y[1] == 1) {
          // also always-true
          ALWAYS_TRUE_PRED();
        }
        return;
      }
    case OP_BO: {
      // Mirror R-level `%(between)%`:
      //   c(NA, NA) -> rep(TRUE, length(x))
      //   c(NA, b)  -> x < b
      //   c(a, NA)  -> x > a
      //   else      -> x > a && x < b   (empty if b <= a, but the
      //                                  per-element compare gives that
      //                                  result automatically)
      const bool y0_NA = (y[0] == NA_LOGICAL);
      const bool y1_NA = (y[1] == NA_LOGICAL);
      if (y0_NA && y1_NA) ALWAYS_TRUE_PRED();
      if (y0_NA) {
        const int yy1 = y[1];
        FORLOOP({ MASK_COMBINE(i, x[i] < yy1); });
        return;
      }
      if (y1_NA) {
        const int yy0 = y[0];
        FORLOOP({ MASK_COMBINE(i, x[i] > yy0); });
        return;
      }
      const int yy0 = y[0], yy1 = y[1];
      FORLOOP({ MASK_COMBINE(i, x[i] > yy0 && x[i] < yy1); });
      return;
    }
    case OP_BC: {
      // Mirror R-level `%]between[%`:
      //   c(NA, NA) -> rep(TRUE, length(x))
      //   c(NA, b)  -> x >= b
      //   c(a, NA)  -> x <= a
      //   b < a     -> rep(FALSE, length(x))
      //   else      -> x <= a || x >= b
      const bool y0_NA = (y[0] == NA_LOGICAL);
      const bool y1_NA = (y[1] == NA_LOGICAL);
      if (y0_NA && y1_NA) ALWAYS_TRUE_PRED();
      if (y0_NA) {
        const int yy1 = y[1];
        FORLOOP({ MASK_COMBINE(i, x[i] >= yy1); });
        return;
      }
      if (y1_NA) {
        const int yy0 = y[0];
        FORLOOP({ MASK_COMBINE(i, x[i] <= yy0); });
        return;
      }
      const int yy0 = y[0], yy1 = y[1];
      if (yy1 < yy0) ALWAYS_FALSE_PRED();
      FORLOOP({ MASK_COMBINE(i, x[i] <= yy0 || x[i] >= yy1); });
      return;
    }
    }
    // # nocov end
  }
  if (N == M) {
    switch (o) {
    case OP_NE: FORLOOP({ MASK_COMBINE(i, x[i] != y[i]); }); break;
    case OP_EQ: FORLOOP({ MASK_COMBINE(i, x[i] == y[i]); }); break;
    case OP_GE: FORLOOP({ MASK_COMBINE(i, x[i] >= y[i]); }); break;
    case OP_LE: FORLOOP({ MASK_COMBINE(i, x[i] <= y[i]); }); break;
    case OP_GT: FORLOOP({ MASK_COMBINE(i, x[i] >  y[i]); }); break;
    case OP_LT: FORLOOP({ MASK_COMBINE(i, x[i] <  y[i]); }); break;
    }
  }
}

static void KFN(L)(unsigned char * ansp, const int o,
                   const int * x, R_xlen_t N,
                   int nThread) {
  if (o == OP_EQ) {
    FORLOOP({ MASK_COMBINE(i, x[i] == 1); });
  } else {
    FORLOOP({ MASK_COMBINE(i, x[i] != 1); });
  }
}

// Unary raw mask predicate: precomputed 0/non-zero byte vector reused as
// `and3s(..., mask_raw)` or `or3s(..., mask_raw)`. Combine via boolean
// truthiness rather than bitwise `&=` / `|=` so a non-{0,1} byte
// (e.g. 5) can't leak into the result mask.
static void KFN(R)(unsigned char * ansp, const int o,
                   const unsigned char * x, R_xlen_t N,
                   int nThread) {
  if (o == OP_EQ) {
    FORLOOP({ MASK_COMBINE(i, x[i] != 0); });
  } else {
    FORLOOP({ MASK_COMBINE(i, x[i] == 0); });    // # nocov
  }
}

static void KFN(RR)(unsigned char * ansp, const int o,
                    const unsigned char * x, R_xlen_t N,
                    const unsigned char * y, R_xlen_t M,
                    int nThread,
                    int * err) {
  if (M == 1) {
    const unsigned char y0 = y[0];
    switch (o) {
    case OP_NI:
    case OP_NE: FORLOOP({ MASK_COMBINE(i, x[i] != y0); }); break;
    case OP_IN:
    case OP_EQ: FORLOOP({ MASK_COMBINE(i, x[i] == y0); }); break;
    default: *err = UNSUPPORTED_TYPEY;
    }
    return;
  }
  // M != 1: OP_NI / OP_IN scan y[0..M-1] per-row so any M works.
  // OP_NE / OP_EQ are element-wise, so they require M == N; otherwise
  // hand off to the R fallback rather than over-read y.
  switch (o) {
  case OP_NI:
    FORLOOP({
      if (UNRESOLVED(i)) {
        unsigned char xi = x[i];
        bool found = false;
        for (R_xlen_t j = 0; j < M; ++j) {
          if (xi == y[j]) { found = true; break; }
        }
        MASK_COMBINE(i, !found);
      }
    });
    break;
  case OP_NE:
    if (M != N) { *err = UNSUPPORTED_TYPEY; return; }
    FORLOOP({ MASK_COMBINE(i, x[i] != y[i]); });
    break;
  case OP_EQ:
    if (M != N) { *err = UNSUPPORTED_TYPEY; return; }
    FORLOOP({ MASK_COMBINE(i, x[i] == y[i]); });
    break;
  case OP_IN:
    FORLOOP({
      if (UNRESOLVED(i)) {
        unsigned char xi = x[i];
        bool found = false;
        for (R_xlen_t j = 0; j < M; ++j) {
          if (xi == y[j]) { found = true; break; }
        }
        MASK_COMBINE(i, found);
      }
    });
    break;
  default: *err = UNSUPPORTED_TYPEY;
  }
}

static void KFN(RI)(unsigned char * ansp, const int o,
                    const unsigned char * x, R_xlen_t N,
                    const int * y, R_xlen_t M,
                    int nThread,
                    int * err) {
  if (M == 1) {
    if (y[0] == NA_INTEGER) {
      // NA_INTEGER == INT_MIN < 0, so it would otherwise hit the
      // negative-scalar branch below and saturate `>` / `>=` to true,
      // which contradicts the package's NA convention. Apply the
      // convention directly: all comparisons against NA are FALSE in
      // the mask, except `!=` / `%notin%` which are TRUE (mirrors the
      // RD NaN handling).
      switch (o) {
      case OP_NI:
      case OP_NE: ALWAYS_TRUE_PRED();
      default:    ALWAYS_FALSE_PRED();
      }
    }
    if (y[0] < 0 || y[0] > 255) {
      // Raw x is in [0,255]; for an out-of-range scalar y every supported
      // predicate is constant.
      const bool y_lt_zero = y[0] < 0;
      switch (o) {
      case OP_IN:
      case OP_EQ: ALWAYS_FALSE_PRED();           // never equal
      case OP_NI:
      case OP_NE: ALWAYS_TRUE_PRED();            // always not-equal
      case OP_GT:
      case OP_GE:
        if (y_lt_zero) ALWAYS_TRUE_PRED();       // x >= 0 always > negative
        ALWAYS_FALSE_PRED();                     // y > 255 never satisfied
      case OP_LT:
      case OP_LE:
        if (y_lt_zero) ALWAYS_FALSE_PRED();      // x >= 0 never < negative
        ALWAYS_TRUE_PRED();                      // x <= 255 always < y > 255
      default: *err = UNSUPPORTED_TYPEY;
      }
      return;
    }

    const unsigned char y0 = y[0];
    switch (o) {
    case OP_NI:
    case OP_NE: FORLOOP({ MASK_COMBINE(i, x[i] != y0); }); break;
    case OP_IN:
    case OP_EQ: FORLOOP({ MASK_COMBINE(i, x[i] == y0); }); break;
    default: *err = UNSUPPORTED_TYPEY;
    }
    return;
  }
  // M != 1: see KFN(RR) note. OP_NE/OP_EQ require M == N; everything
  // else either scans (OP_NI / OP_IN) or is unsupported.
  switch (o) {
  case OP_NI:
    FORLOOP({
      if (UNRESOLVED(i)) {
        unsigned char xi = x[i];
        bool found = false;
        for (R_xlen_t j = 0; j < M; ++j) {
          if (xi == y[j]) { found = true; break; }
        }
        MASK_COMBINE(i, !found);
      }
    });
    break;
  case OP_NE:
    if (M != N) { *err = UNSUPPORTED_TYPEY; return; }
    FORLOOP({ MASK_COMBINE(i, x[i] != y[i]); });
    break;
  case OP_EQ:
    if (M != N) { *err = UNSUPPORTED_TYPEY; return; }
    FORLOOP({ MASK_COMBINE(i, x[i] == y[i]); });
    break;
  case OP_IN:
    FORLOOP({
      if (UNRESOLVED(i)) {
        unsigned char xi = x[i];
        bool found = false;
        for (R_xlen_t j = 0; j < M; ++j) {
          if (xi == y[j]) { found = true; break; }
        }
        MASK_COMBINE(i, found);
      }
    });
    break;
  default: *err = UNSUPPORTED_TYPEY;
  }
}

static void KFN(RD)(unsigned char * ansp, const int o,
                    const unsigned char * x, R_xlen_t N,
                    const double * y, R_xlen_t M,
                    int nThread,
                    int * err) {
  if (M == 1) {
    if (ISNAN(y[0])) {
      // Package convention (NA -> FALSE in mask): NaN comparisons are
      // FALSE for every op except `!=` / `%notin%` which are TRUE.
      // See ?and3s "Note on NA / NaN".
      switch (o) {
      case OP_NI:
      case OP_NE: ALWAYS_TRUE_PRED();
      default: ALWAYS_FALSE_PRED();
      }
    }
    if (y[0] >= 0 && y[0] <= 255 && y[0] != (int)y[0]) {
      // Non-integer in [0,255]: == / != are constant; order ops fall back.
      switch (o) {
      case OP_IN:
      case OP_EQ: ALWAYS_FALSE_PRED();
      case OP_NI:
      case OP_NE: ALWAYS_TRUE_PRED();
      default: *err = UNSUPPORTED_TYPEY;
      }
      return;
    }
    if (y[0] < 0 || y[0] > 255) {
      const bool y_lt_zero = y[0] < 0;
      switch (o) {
      case OP_IN:
      case OP_EQ: ALWAYS_FALSE_PRED();
      case OP_NI:
      case OP_NE: ALWAYS_TRUE_PRED();
      case OP_GT:
      case OP_GE:
        if (y_lt_zero) ALWAYS_TRUE_PRED();
        ALWAYS_FALSE_PRED();
      case OP_LT:
      case OP_LE:
        if (y_lt_zero) ALWAYS_FALSE_PRED();
        ALWAYS_TRUE_PRED();
      default: *err = UNSUPPORTED_TYPEY;
      }
      return;
    }
    const unsigned char y0 = (unsigned char)y[0];
    switch (o) {
    case OP_NI:
    case OP_NE: FORLOOP({ MASK_COMBINE(i, x[i] != y0); }); break;
    case OP_IN:
    case OP_EQ: FORLOOP({ MASK_COMBINE(i, x[i] == y0); }); break;
    default: *err = UNSUPPORTED_TYPEY;
    }
    return;
  }
  // M != 1: see KFN(RR) note. OP_NE/OP_EQ require M == N.
  switch (o) {
  case OP_NI:
    FORLOOP({
      if (UNRESOLVED(i)) {
        unsigned char xi = x[i];
        bool found = false;
        for (R_xlen_t j = 0; j < M; ++j) {
          if (xi == y[j]) { found = true; break; }
        }
        MASK_COMBINE(i, !found);
      }
    });
    break;
  case OP_NE:
    if (M != N) { *err = UNSUPPORTED_TYPEY; return; }
    FORLOOP({ MASK_COMBINE(i, x[i] != y[i]); });
    break;
  case OP_EQ:
    if (M != N) { *err = UNSUPPORTED_TYPEY; return; }
    FORLOOP({ MASK_COMBINE(i, x[i] == y[i]); });
    break;
  case OP_IN:
    FORLOOP({
      if (UNRESOLVED(i)) {
        unsigned char xi = x[i];
        bool found = false;
        for (R_xlen_t j = 0; j < M; ++j) {
          if (xi == y[j]) { found = true; break; }
        }
        MASK_COMBINE(i, found);
      }
    });
    break;
  default: *err = UNSUPPORTED_TYPEY;
  }
}

static void KFN(SeqS1)(unsigned char * ansp, const SEXP * xp, R_xlen_t N,
                       const char * y, const int ny) {
  for (R_xlen_t i = 0; i < N; ++i) {
    if (!UNRESOLVED(i)) continue;
    int nxi = length(xp[i]);
    if (nxi != ny) {
      MASK_COMBINE(i, 0);
      continue;
    }
    const char * xi = CHAR(xp[i]);
    MASK_COMBINE(i, string_equal(xi, y));
  }
}

static void KFN(SneqS1)(unsigned char * ansp, const SEXP * xp, R_xlen_t N,
                        const char * y, const int ny) {
  for (R_xlen_t i = 0; i < N; ++i) {
    if (!UNRESOLVED(i)) continue;
    int nxi = length(xp[i]);
    if (nxi != ny) {
      MASK_COMBINE(i, 1);
      continue;
    }
    const char * xi = CHAR(xp[i]);
    MASK_COMBINE(i, !string_equal(xi, y));
  }
}

static void KFN(SeqS)(unsigned char * ansp, const SEXP * xp, R_xlen_t N,
                      const SEXP * yp) {
  for (R_xlen_t i = 0; i < N; ++i) {
    if (!UNRESOLVED(i)) continue;
    MASK_COMBINE(i, string_equal(CHAR(xp[i]), CHAR(yp[i])));
  }
}

static void KFN(SneqS)(unsigned char * ansp, const SEXP * xp, R_xlen_t N,
                       const SEXP * yp) {
  for (R_xlen_t i = 0; i < N; ++i) {
    if (!UNRESOLVED(i)) continue;
    MASK_COMBINE(i, !string_equal(CHAR(xp[i]), CHAR(yp[i])));
  }
}

static void KFN(SS)(unsigned char * ansp, const int o,
                    const SEXP * x, R_xlen_t N,
                    const SEXP * y, R_xlen_t M) {
  if (M == 1) {
    if (o == OP_EQ) {
      KFN(SeqS1)(ansp, x, N, CHAR(y[0]), length(y[0]));
    } else {
      KFN(SneqS1)(ansp, x, N, CHAR(y[0]), length(y[0]));
    }
  } else {
    if (o == OP_EQ) {
      KFN(SeqS)(ansp, x, N, y);
    } else {
      KFN(SneqS)(ansp, x, N, y);
    }
  }
}

// Top-level dispatcher: parameter-types -> kernel.
static void KFN(dispatch)(unsigned char * ansp, const int o,
                          SEXP x, SEXP y, int nThread,
                          int * err) {
  R_xlen_t N = xlength(x);
  R_xlen_t M = xlength(y);

  // The non-raw element-wise kernels handle comparison ops with scalar
  // or recycled RHS and the public 2-element between-family operators.
  // Any other op/shape must defer to the R fallback; otherwise a switch
  // with no matching case can silently leave the mask unchanged.
  const bool m_supported_nonraw = vops_nonraw_supported(o, M, N);

  switch (TYPEOF(x)) {
  case LGLSXP:
    switch (TYPEOF(y)) {
    case LGLSXP:
      if (!m_supported_nonraw) { *err = UNSUPPORTED_TYPEY; break; }
      KFN(LL)(ansp, o, LOGICAL(x), N, LOGICAL(y), M, nThread);
      break;
    case NILSXP:
      if (!vops_op_is_unary_mask(o)) { *err = UNSUPPORTED_TYPEY; break; }
      KFN(L)(ansp, o, LOGICAL(x), N, nThread);
      break;
    default:
      *err = UNSUPPORTED_TYPEY;
    }
    break;
  case INTSXP:
    if (!m_supported_nonraw) { *err = UNSUPPORTED_TYPEY; break; }
    switch (TYPEOF(y)) {
    case INTSXP:
      KFN(II)(ansp, o, INTEGER(x), N, INTEGER(y), M, nThread);
      break;
    case REALSXP:
      KFN(ID)(ansp, o, INTEGER(x), N, REAL(y), M, nThread);
      break;
    default:
      *err = UNSUPPORTED_TYPEY;
    }
    break;
  case REALSXP:
    if (!m_supported_nonraw) { *err = UNSUPPORTED_TYPEY; break; }
    switch (TYPEOF(y)) {
    case INTSXP:
      KFN(DI)(ansp, o, REAL(x), N, INTEGER(y), M, nThread);
      break;
    case REALSXP:
      KFN(DD)(ansp, o, REAL(x), N, REAL(y), M, nThread);
      break;
    default:
      *err = UNSUPPORTED_TYPEY;
    }
    break;
  case RAWSXP:
    switch (TYPEOF(y)) {
    case RAWSXP:
      KFN(RR)(ansp, o, RAW(x), N, RAW(y), M, nThread, err);
      break;
    case INTSXP:
      KFN(RI)(ansp, o, RAW(x), N, INTEGER(y), M, nThread, err);
      break;
    case REALSXP:
      KFN(RD)(ansp, o, RAW(x), N, REAL(y), M, nThread, err);
      break;
    case NILSXP:
      // Unary raw mask: `and3s(..., mask_raw)` / `or3s(..., mask_raw)`.
      // The wrapper sets oo to "==" for a bare symbol, so o is OP_EQ.
      if (!vops_op_is_unary_mask(o)) { *err = UNSUPPORTED_TYPEY; break; }
      KFN(R)(ansp, o, RAW(x), N, nThread);
      break;
    default:
      // raw vs logical / character / etc.: defer to the R fallback so
      // base R coercion / equality semantics decide. The pre-Phase-3
      // code dispatched to a unary raw kernel here, which silently
      // ignored y and returned the truthiness of x.
      *err = UNSUPPORTED_TYPEY;
    }
    break;
  case STRSXP:
    if (TYPEOF(y) == STRSXP && (o == OP_EQ || o == OP_NE)) {
      // KFN(SS) handles M == 1 and M == N only; other M would over-read.
      if (M != 1 && M != N) { *err = UNSUPPORTED_TYPEY; break; }
      KFN(SS)(ansp, o, STRING_PTR_RO(x), N, STRING_PTR_RO(y), M);
    } else {
      *err = UNSUPPORTED_TYPEY;
    }
    break;
  default:
    *err = UNSUPPORTED_TYPEX;
  }
}

// Allow the file to be re-included with a different VOPS_* tag.
#undef KFN
#undef KFN_
#undef MASK_COMBINE
#undef ALWAYS_TRUE_PRED
#undef ALWAYS_FALSE_PRED
#undef UNRESOLVED
#undef ANNIHILATOR_VAL
#undef UNSUPPORTED_TYPEX
#undef UNSUPPORTED_TYPEY
#undef ORAND_TAG
#undef FORLOOP_combine_op
#undef VOPS_AND
#undef VOPS_OR
#undef VOPS_INIT
