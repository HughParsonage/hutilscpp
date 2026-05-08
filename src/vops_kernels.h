// Phase-3 shared dispatch kernels for `and3s` / `or3s`.
//
// Parameterised on VOPS_AND or VOPS_OR -- the two kernels differ only by
// their combine operator (`&=` vs `|=`) and the corresponding identity /
// annihilator. Include this header from Cand3s.c with VOPS_AND and from
// Cor3s.c with VOPS_OR; each translation unit gets its own file-static
// copy of the kernels (vand2s_* or vor2s_*).
//
// Macros defined here:
//   KFN(name)           -> vand2s_<name> or vor2s_<name>
//   MASK_COMBINE(i, e)  -> ansp[i] &= (e)  | ansp[i] |= (e)
//   ALWAYS_TRUE_PRED()  -> AND: return; OR: memset 1 + return
//   ALWAYS_FALSE_PRED() -> AND: memset 0 + return; OR: return
//   UNRESOLVED(i)       -> mask still mutable (1 for AND, 0 for OR)
//   ANNIHILATOR_VAL     -> 0 (AND) | 1 (OR)
//   UNSUPPORTED_TYPEX/Y -> AND3_*  | OR3__*
//   ORAND_TAG           -> ORAND_AND | ORAND_OR (passed to uc_betweenidd)

#if !defined(VOPS_AND) && !defined(VOPS_OR)
#error "vops_kernels.h requires VOPS_AND or VOPS_OR to be defined"
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
#else
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

#define KFN(name) KFN_(name)

#define FORLOOP_combine_op(op, rhs)                                 \
  FORLOOP({ MASK_COMBINE(i, x[i] op (rhs)); })

static void KFN(II)(unsigned char * ansp, const int o,
                    const int * x, R_xlen_t N,
                    const int * y, R_xlen_t M,
                    int nThread) {
  if (M == 2 && op_xlen2(o)) {
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
        // Unusual x %between% c(NA, NA): no constraint either way.
        return; // # nocov
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
    int y0 = y[0];
    int y1 = y[1];
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
    switch (o) {
    case OP_BW:
      FORLOOP({ MASK_COMBINE(i, (x[i] >= y[0]) && (x[i] <= y[1])); });
      return;
    case OP_BO:
      FORLOOP({ MASK_COMBINE(i, (x[i] > y[0]) && (x[i] < y[1])); });
      return;
    case OP_BC:
      FORLOOP({ MASK_COMBINE(i, (x[i] <= y[0]) || (x[i] >= y[1])); });
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
    case OP_BO:
    case OP_BC:
      // Legacy: pre-Phase-3 kernels for logical x in {0,1} returned
      // unconditionally for these ops, regardless of orientation. The
      // call doesn't reach C in mainstream paths (BO between open and
      // BC complement on logical are unusual), but a few coverage tests
      // exercise it; the no-op preserves the historical contract.
      return;
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

  // The non-raw element-wise kernels handle three RHS shapes: scalar
  // (M == 1), recycled element-wise (M == N), and the 2-element bounds
  // forms used by between operators (M == 2 with op_xlen2(o)). Any
  // other M silently fell through pre-fix, leaving the AND mask all-1
  // (or the OR mask all-0) and producing wrong public results. Defer
  // those shapes to the R fallback. Raw kernels self-validate M and
  // are excluded.
  const bool m_supported_nonraw =
      M == 1 || M == N || (M == 2 && op_xlen2(o));

  switch (TYPEOF(x)) {
  case LGLSXP:
    switch (TYPEOF(y)) {
    case LGLSXP:
      if (!m_supported_nonraw) { *err = UNSUPPORTED_TYPEY; break; }
      KFN(LL)(ansp, o, LOGICAL(x), N, LOGICAL(y), M, nThread);
      break;
    default:
      KFN(L)(ansp, o, LOGICAL(x), N, nThread);
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
