#include "hutilscpp.h"
#include <string.h>
#include <stdlib.h>

// row_id_by_pattern: dense group ids for the rows of a data.frame, keyed on a
// cheap per-cell "pattern" (zero/nonzero, magnitude bucket, factor code, or
// CHARSXP identity). Each row is reduced to a 128-bit hash (two independent
// 64-bit chains) in a blocked, column-inner pass; per-thread hash tables assign
// local ids which are merged, ranked by (count desc, first row asc) and
// remapped to the final integer ids.

#define RIBP_BLOCK 2048
#define RIBP_MIN_ROWS_PER_THREAD 4096

#define RIBP_KIND_ONE_INT 0
#define RIBP_KIND_ONE_DBL 1
#define RIBP_KIND_ONE_RAW 2
#define RIBP_KIND_FACTOR  3
#define RIBP_KIND_CHAR    4
#define RIBP_KIND_MAG_INT 5
#define RIBP_KIND_MAG_DBL 6
#define RIBP_N_KINDS      7

static const int ribp_width[RIBP_N_KINDS] = {1, 1, 1, 32, 64, 7, 13};

#if defined _OPENMP && _OPENMP >= 201511
#define RIBP_SIMD _Pragma("omp simd")
#else
#define RIBP_SIMD
#endif

#if defined(__GNUC__) || defined(__clang__)
#define RIBP_PREFETCH(addr) __builtin_prefetch((addr), 0, 1)
#else
#define RIBP_PREFETCH(addr) do {} while (0)
#endif
#define RIBP_PREFETCH_AHEAD 8

#define RIBP_SEED1 UINT64_C(0x243f6a8885a308d3)
#define RIBP_SEED2 UINT64_C(0x13198a2e03707344)
#define RIBP_GOLD  UINT64_C(0x9e3779b97f4a7c15)

// ---------------------------------------------------------------------------
// Hash mixers

static inline uint64_t ribp_mix1(uint64_t z) {
  // splitmix64 finaliser
  z ^= z >> 30;
  z *= UINT64_C(0xbf58476d1ce4e5b9);
  z ^= z >> 27;
  z *= UINT64_C(0x94d049bb133111eb);
  return z ^ (z >> 31);
}

static inline uint64_t ribp_mix2(uint64_t z) {
  // murmur3 fmix64
  z ^= z >> 33;
  z *= UINT64_C(0xff51afd7ed558ccd);
  z ^= z >> 33;
  z *= UINT64_C(0xc4ceb9fe1a85ec53);
  return z ^ (z >> 33);
}

// ---------------------------------------------------------------------------
// Kernels: consume one column slab of n rows into the packed accumulators.

static void ribp_fold(uint64_t * restrict acc, uint64_t * restrict h1, uint64_t * restrict h2, int n) {
  for (int r = 0; r < n; ++r) {
    uint64_t w = acc[r];
    h1[r] = ribp_mix1(h1[r] ^ w);
    h2[r] = ribp_mix2(h2[r] + w * RIBP_GOLD);
    acc[r] = 0;
  }
}

#define RIBP_ONE_INT_KERNEL(NAME, NAIS)                                     \
static void NAME(uint64_t * restrict acc, const int * restrict x, int n) {  \
  RIBP_SIMD                                                                 \
  for (int r = 0; r < n; ++r) {                                             \
    int xr = x[r];                                                          \
    uint64_t bit = (xr != 0) & ((xr != NA_INTEGER) | (NAIS));               \
    acc[r] = (acc[r] << 1) | bit;                                           \
  }                                                                         \
}
RIBP_ONE_INT_KERNEL(ribp_one_int_na0, 0)
RIBP_ONE_INT_KERNEL(ribp_one_int_na1, 1)

#define RIBP_ONE_DBL_KERNEL(NAME, NAIS)                                        \
static void NAME(uint64_t * restrict acc, const double * restrict x, int n) {  \
  RIBP_SIMD                                                                    \
  for (int r = 0; r < n; ++r) {                                                \
    double xr = x[r];                                                          \
    uint64_t bit = (xr != 0.0) & ((xr == xr) | (NAIS));                        \
    acc[r] = (acc[r] << 1) | bit;                                              \
  }                                                                            \
}
RIBP_ONE_DBL_KERNEL(ribp_one_dbl_na0, 0)
RIBP_ONE_DBL_KERNEL(ribp_one_dbl_na1, 1)

static void ribp_one_raw(uint64_t * restrict acc, const unsigned char * restrict x, int n) {
  RIBP_SIMD
  for (int r = 0; r < n; ++r) {
    uint64_t bit = x[r] != 0;
    acc[r] = (acc[r] << 1) | bit;
  }
}

static void ribp_factor(uint64_t * restrict acc, const int * restrict x, int n) {
  RIBP_SIMD
  for (int r = 0; r < n; ++r) {
    acc[r] = (acc[r] << 32) | (uint64_t) (uint32_t) x[r];
  }
}

static void ribp_char(uint64_t * restrict acc, const SEXP * restrict x, int n) {
  for (int r = 0; r < n; ++r) {
    acc[r] = (uint64_t) (uintptr_t) x[r];
  }
}

// Magnitude of an integer: 0 = zero, 1..31 = floor(log2(x)) + 1 for x > 0,
// 33..63 for x < 0 (by |x|), NA -> na_code (64 when na_is = 1, 0 otherwise).
// Two passes over the block so that both vectorise: |x| is converted to a
// double (exact for every 32-bit integer) and the exponent field is read back.
typedef union {
  double d;
  uint64_t u;
} ribp_du;

static void ribp_mag_int(uint64_t * restrict acc, const int * restrict x, int n, uint32_t na_code) {
  ribp_du buf[RIBP_BLOCK];
  RIBP_SIMD
  for (int r = 0; r < n; ++r) {
    uint32_t ux = (uint32_t) x[r];
    uint32_t neg = ux >> 31;
    uint32_t ax = (ux ^ (0u - neg)) + neg; // |x|; INT_MIN -> 2^31
    buf[r].d = (double) ax;
  }
  RIBP_SIMD
  for (int r = 0; r < n; ++r) {
    int xr = x[r];
    uint32_t neg = (uint32_t) xr >> 31;
    uint32_t e = (uint32_t) (buf[r].u >> 52);     // 0 for zero, 1023 + floor(log2|x|) otherwise
    uint32_t nzmask = 0u - (uint32_t) (e != 0);
    uint32_t m = (e - 1022u) & nzmask;            // 1..32 (32 only for INT_MIN, i.e. NA)
    uint32_t code = m + 32u * neg;
    uint32_t namask = 0u - (uint32_t) (xr == NA_INTEGER);
    code = (na_code & namask) | (code & ~namask);
    acc[r] = (acc[r] << 7) | code;
  }
}

// Magnitude of a double: 0 = zero (either sign), NaN/NA -> na_code (1 when
// na_is = 1, 0 otherwise), otherwise 2 + bucket + 2100 * sign where bucket =
// floor(log2(|x|)) + 1075 (1..52 subnormal, 53..2098 normal, 2099 Inf).
// Subnormals are scaled by 2^52 (exact) so that their exponent field can be
// read directly; the arithmetic is done on the 32-bit high/low words so that
// it vectorises without 64-bit lane comparisons.
static void ribp_mag_dbl(uint64_t * restrict acc, const double * restrict x, int n, uint32_t na_code) {
  uint32_t hi[RIBP_BLOCK];
  uint32_t lo[RIBP_BLOCK];
  uint32_t hs[RIBP_BLOCK];
  ribp_du bs[RIBP_BLOCK];
  RIBP_SIMD
  for (int r = 0; r < n; ++r) {
    bs[r].d = x[r] * 4503599627370496.0; // 2^52
  }
  RIBP_SIMD
  for (int r = 0; r < n; ++r) {
    uint64_t u;
    memcpy(&u, &x[r], 8);
    hi[r] = (uint32_t) (u >> 32);
    lo[r] = (uint32_t) u;
    hs[r] = (uint32_t) (bs[r].u >> 32);
  }
  RIBP_SIMD
  for (int r = 0; r < n; ++r) {
    uint32_t h = hi[r];
    uint32_t sign = h >> 31;
    uint32_t e = (h >> 20) & 0x7ffu;
    uint32_t es = (hs[r] >> 20) & 0x7ffu;
    uint32_t emask = 0u - (uint32_t) (e != 0);
    uint32_t bucket = ((e + 52u) & emask) | (es & ~emask);
    uint32_t val = 2u + bucket + 2100u * sign;
    uint32_t mant_nz = (uint32_t) (((h & 0xfffffu) | lo[r]) != 0);
    uint32_t nanmask = 0u - ((uint32_t) (e == 0x7ffu) & mant_nz);
    uint32_t zmask = 0u - (uint32_t) (((h & 0x7fffffffu) | lo[r]) != 0);
    uint32_t code = ((val & ~nanmask) | (na_code & nanmask)) & zmask;
    acc[r] = (acc[r] << 13) | (uint64_t) code;
  }
}

// ---------------------------------------------------------------------------
// Hash table: dense entries in insertion order + open-addressed slot index.

typedef struct {
  uint64_t h1;
  uint64_t h2;
  uint64_t first_row;
  uint64_t count;
} ribp_entry;

typedef struct {
  ribp_entry * entries;   // dense, insertion order; id = index
  uint32_t * slots;       // entry index + 1; 0 = empty
  uint64_t nslots;        // power of two
  uint64_t nused;
  uint64_t capacity;      // entries capacity
  int k;                  // log2(nslots)
} ribp_table;

static int ribp_table_init(ribp_table * t, int k, uint64_t capacity) {
  t->k = k;
  t->nslots = UINT64_C(1) << k;
  t->nused = 0;
  t->capacity = capacity < 1024 ? 1024 : capacity;
  t->slots = calloc(t->nslots, sizeof(uint32_t));
  t->entries = malloc(t->capacity * sizeof(ribp_entry));
  if (t->slots == NULL || t->entries == NULL) {
    free(t->slots);
    free(t->entries);
    t->slots = NULL;
    t->entries = NULL;
    return 1;
  }
  return 0;
}

static void ribp_table_free(ribp_table * t) {
  free(t->slots);
  free(t->entries);
  t->slots = NULL;
  t->entries = NULL;
}

static int ribp_table_grow(ribp_table * t) {
  int k = t->k + 1;
  uint64_t nslots = UINT64_C(1) << k;
  uint32_t * slots = calloc(nslots, sizeof(uint32_t));
  if (slots == NULL) {
    return 1;
  }
  uint64_t mask = nslots - 1;
  int shift = 64 - k;
  for (uint64_t i = 0; i < t->nused; ++i) {
    uint64_t s = t->entries[i].h1 >> shift;
    while (slots[s] != 0) {
      s = (s + 1) & mask;
    }
    slots[s] = (uint32_t) (i + 1);
  }
  free(t->slots);
  t->slots = slots;
  t->nslots = nslots;
  t->k = k;
  return 0;
}

// Returns the entry index, or UINT32_MAX on failure (allocation or overflow).
static uint32_t ribp_insert(ribp_table * t, uint64_t h1, uint64_t h2, uint64_t row, uint64_t count) {
  uint64_t mask = t->nslots - 1;
  uint64_t s = h1 >> (64 - t->k);
  for (;;) {
    uint32_t e = t->slots[s];
    if (e == 0) {
      break;
    }
    ribp_entry * E = &t->entries[e - 1];
    if (E->h1 == h1 && E->h2 == h2) {
      E->count += count;
      if (row < E->first_row) {
        E->first_row = row;
      }
      return e - 1;
    }
    s = (s + 1) & mask;
  }
  // New entry
  if (t->nused >= UINT32_MAX - 1) {
    return UINT32_MAX; // # nocov
  }
  if (t->nused == t->capacity) {
    uint64_t capacity = t->capacity * 2;
    ribp_entry * entries = realloc(t->entries, capacity * sizeof(ribp_entry));
    if (entries == NULL) {
      return UINT32_MAX; // # nocov
    }
    t->entries = entries;
    t->capacity = capacity;
  }
  uint32_t id = (uint32_t) t->nused;
  t->entries[id].h1 = h1;
  t->entries[id].h2 = h2;
  t->entries[id].first_row = row;
  t->entries[id].count = count;
  t->slots[s] = id + 1;
  t->nused++;
  if (t->nused * 10 > t->nslots * 7) {
    if (ribp_table_grow(t)) {
      return UINT32_MAX; // # nocov
    }
  }
  return id;
}

// ---------------------------------------------------------------------------
// Column descriptors and per-thread state

typedef struct {
  const void * p;
  int kind;
  int fold_before;
} ribp_col;

typedef struct {
  ribp_table tbl;
  R_xlen_t begin;
  R_xlen_t end;
  uint64_t nlocal;
  int failed;
  char pad[64];
} ribp_thread;

typedef struct {
  ribp_col * cols;
  ribp_thread * threads;
  int nthreads;
  ribp_table * parts;
  int nparts;
  uint64_t ** remap;
  uint32_t ** order;      // per thread: local entry index of each sorted entry
  ribp_entry ** sorted;   // per thread: local entries copied in partition order
  uint64_t * bucket_off;  // per thread: T + 1 partition offsets into sorted/order
  uint64_t * offsets;
  uint64_t * keys;
  uint64_t * keys2;
  uint32_t * vals;
  uint32_t * vals2;
  int * final_id;
} ribp_ctx;

static void ribp_ctx_init(ribp_ctx * c) {
  memset(c, 0, sizeof(*c));
}

static void ribp_ctx_free(ribp_ctx * c) {
  free(c->cols);
  if (c->threads != NULL) {
    for (int t = 0; t < c->nthreads; ++t) {
      ribp_table_free(&c->threads[t].tbl);
    }
    free(c->threads);
  }
  if (c->parts != NULL) {
    for (int q = 0; q < c->nparts; ++q) {
      ribp_table_free(&c->parts[q]);
    }
    free(c->parts);
  }
  if (c->remap != NULL) {
    for (int t = 0; t < c->nthreads; ++t) {
      free(c->remap[t]);
    }
    free(c->remap);
  }
  if (c->order != NULL) {
    for (int t = 0; t < c->nthreads; ++t) {
      free(c->order[t]);
    }
    free(c->order);
  }
  if (c->sorted != NULL) {
    for (int t = 0; t < c->nthreads; ++t) {
      free(c->sorted[t]);
    }
    free(c->sorted);
  }
  free(c->bucket_off);
  free(c->offsets);
  free(c->keys);
  free(c->keys2);
  free(c->vals);
  free(c->vals2);
  free(c->final_id);
  ribp_ctx_init(c);
}

static void ribp_thread_range(R_xlen_t n, int thread, int team_size, R_xlen_t * begin, R_xlen_t * end) {
  const R_xlen_t chunk = n / team_size;
  const R_xlen_t remainder = n % team_size;
  const R_xlen_t extra_before = thread < remainder ? thread : remainder;
  *begin = thread * chunk + extra_before;
  *end = *begin + chunk + (thread < remainder);
}

static int ribp_effective_threads(R_xlen_t n, int requested) {
#if defined _OPENMP && _OPENMP >= 201511
  if (requested <= 1 || n < 2 * RIBP_MIN_ROWS_PER_THREAD) {
    return 1;
  }
  R_xlen_t useful = n / RIBP_MIN_ROWS_PER_THREAD;
  return useful < requested ? (int) useful : requested;
#else
  (void) n;
  (void) requested;
  return 1;
#endif
}

// Slot count for a table that will hold at most `expected` entries: the
// smallest power of two with at least 2 * expected slots (load <= 0.5, so no
// growth is needed unless the estimate is exceeded). Untouched calloc pages
// cost nothing, so over-sizing for tables that stay small is cheap.
static int ribp_slots_k(uint64_t expected) {
  int k = 10;
  while (k < 40 && (UINT64_C(1) << k) < 2 * expected) {
    ++k;
  }
  return k;
}

// ---------------------------------------------------------------------------
// Pass 1: hash rows in blocks and insert into the thread-local table.

static void ribp_hash_rows(const ribp_col * cols, int ncol, int na_is, ribp_thread * th, uint32_t * outu) {
  uint64_t acc[RIBP_BLOCK];
  uint64_t h1[RIBP_BLOCK];
  uint64_t h2[RIBP_BLOCK];
  const uint32_t na_code_int = na_is ? 64u : 0u;
  const uint32_t na_code_dbl = na_is ? 1u : 0u;
  int final_fold = 0;
  {
    int nb = 0;
    for (int j = 0; j < ncol; ++j) {
      if (cols[j].fold_before) {
        nb = 0;
      }
      nb += ribp_width[cols[j].kind];
    }
    final_fold = nb > 0;
  }
  for (R_xlen_t b = th->begin; b < th->end; b += RIBP_BLOCK) {
    R_xlen_t remaining = th->end - b;
    int n = remaining < RIBP_BLOCK ? (int) remaining : RIBP_BLOCK;
    for (int r = 0; r < n; ++r) {
      acc[r] = 0;
      h1[r] = RIBP_SEED1;
      h2[r] = RIBP_SEED2;
    }
    for (int j = 0; j < ncol; ++j) {
      const ribp_col * col = &cols[j];
      if (col->fold_before) {
        ribp_fold(acc, h1, h2, n);
      }
      switch (col->kind) {
      case RIBP_KIND_ONE_INT:
        if (na_is) {
          ribp_one_int_na1(acc, (const int *) col->p + b, n);
        } else {
          ribp_one_int_na0(acc, (const int *) col->p + b, n);
        }
        break;
      case RIBP_KIND_ONE_DBL:
        if (na_is) {
          ribp_one_dbl_na1(acc, (const double *) col->p + b, n);
        } else {
          ribp_one_dbl_na0(acc, (const double *) col->p + b, n);
        }
        break;
      case RIBP_KIND_ONE_RAW:
        ribp_one_raw(acc, (const unsigned char *) col->p + b, n);
        break;
      case RIBP_KIND_FACTOR:
        ribp_factor(acc, (const int *) col->p + b, n);
        break;
      case RIBP_KIND_CHAR:
        ribp_char(acc, (const SEXP *) col->p + b, n);
        break;
      case RIBP_KIND_MAG_INT:
        ribp_mag_int(acc, (const int *) col->p + b, n, na_code_int);
        break;
      case RIBP_KIND_MAG_DBL:
        ribp_mag_dbl(acc, (const double *) col->p + b, n, na_code_dbl);
        break;
      }
    }
    if (final_fold) {
      ribp_fold(acc, h1, h2, n);
    }
    for (int r = 0; r < n; ++r) {
      if (r + RIBP_PREFETCH_AHEAD < n) {
        RIBP_PREFETCH(&th->tbl.slots[h1[r + RIBP_PREFETCH_AHEAD] >> (64 - th->tbl.k)]);
      }
      uint32_t id = ribp_insert(&th->tbl, h1[r], h2[r], (uint64_t) (b + r), 1);
      if (id == UINT32_MAX) {
        th->failed = 1; // # nocov
        return;         // # nocov
      }
      outu[b + r] = id;
    }
  }
}

// ---------------------------------------------------------------------------
// Ordering of groups by (count desc, first_row asc)

typedef struct {
  uint64_t count;
  uint64_t first;
  uint64_t gid;
} ribp_group;

static int ribp_group_cmp(const void * a, const void * b) {
  const ribp_group * A = a;
  const ribp_group * B = b;
  if (A->count != B->count) {
    return A->count > B->count ? -1 : 1;
  }
  if (A->first != B->first) {
    return A->first < B->first ? -1 : 1;
  }
  return 0;
}

// ---------------------------------------------------------------------------

static void ribp_error(ribp_ctx * ctx, const char * msg) {
  ribp_ctx_free(ctx);
  error("%s", msg);
}

SEXP Crow_id_by_pattern(SEXP DT, SEXP Kinds, SEXP NaIs, SEXP MaxPatterns, SEXP nthreads) {
  if (TYPEOF(DT) != VECSXP) {
    error("Internal error(Crow_id_by_pattern): DT is not a list."); // # nocov
  }
  R_xlen_t ncol_x = xlength(DT);
  if (ncol_x == 0 || ncol_x > INT_MAX) {
    error("Internal error(Crow_id_by_pattern): unsupported number of columns."); // # nocov
  }
  int ncol = (int) ncol_x;
  if (TYPEOF(Kinds) != INTSXP || xlength(Kinds) != ncol_x) {
    error("Internal error(Crow_id_by_pattern): kinds malformed."); // # nocov
  }
  const int na_is = asInteger2(NaIs) != 0;
  double max_patterns_d = asReal(MaxPatterns);
  if (ISNAN(max_patterns_d) || max_patterns_d < 1) {
    error("Internal error(Crow_id_by_pattern): max_patterns malformed."); // # nocov
  }
  uint64_t max_patterns = max_patterns_d >= 2147483647.0 ? UINT64_C(2147483647) : (uint64_t) max_patterns_d;
  int requested = as_nThread(nthreads);

  const int * kinds = INTEGER(Kinds);
  R_xlen_t N = xlength(VECTOR_ELT(DT, 0));

  ribp_ctx ctx;
  ribp_ctx_init(&ctx);

  ctx.cols = malloc(sizeof(ribp_col) * ncol);
  if (ctx.cols == NULL) {
    ribp_error(&ctx, "Unable to allocate column descriptors."); // # nocov
  }
  {
    int nb = 0;
    for (int j = 0; j < ncol; ++j) {
      SEXP xj = VECTOR_ELT(DT, j);
      int kind = kinds[j];
      if (kind < 0 || kind >= RIBP_N_KINDS) {
        ribp_error(&ctx, "Internal error(Crow_id_by_pattern): unknown kind."); // # nocov
      }
      if (xlength(xj) != N) {
        ribp_error(&ctx, "Internal error(Crow_id_by_pattern): columns have unequal lengths."); // # nocov
      }
      int type = TYPEOF(xj);
      const void * p = NULL;
      switch (kind) {
      case RIBP_KIND_ONE_INT:
        if (type == INTSXP) {
          p = INTEGER(xj);
        } else if (type == LGLSXP) {
          p = LOGICAL(xj);
        }
        break;
      case RIBP_KIND_FACTOR:
      case RIBP_KIND_MAG_INT:
        if (type == INTSXP) {
          p = INTEGER(xj);
        }
        break;
      case RIBP_KIND_ONE_DBL:
      case RIBP_KIND_MAG_DBL:
        if (type == REALSXP) {
          p = REAL(xj);
        }
        break;
      case RIBP_KIND_ONE_RAW:
        if (type == RAWSXP) {
          p = RAW(xj);
        }
        break;
      case RIBP_KIND_CHAR:
        if (type == STRSXP) {
          p = STRING_PTR_RO(xj);
        }
        break;
      }
      if (p == NULL) {
        ribp_error(&ctx, "Internal error(Crow_id_by_pattern): column type does not match its kind."); // # nocov
      }
      int width = ribp_width[kind];
      int fold_before = nb + width > 64;
      if (fold_before) {
        nb = 0;
      }
      nb += width;
      ctx.cols[j].p = p;
      ctx.cols[j].kind = kind;
      ctx.cols[j].fold_before = fold_before;
    }
  }

  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * ansp = INTEGER(ans);
  if (N == 0) {
    ribp_ctx_free(&ctx);
    UNPROTECT(1);
    return ans;
  }
  uint32_t * outu = (uint32_t *) ansp;

  // ---- Pass 1 -------------------------------------------------------------
  int T = ribp_effective_threads(N, requested);
  ctx.threads = calloc(T, sizeof(ribp_thread));
  if (ctx.threads == NULL) {
    ribp_error(&ctx, "Unable to allocate thread state."); // # nocov
  }
  ctx.nthreads = T;
  int team = T;
  int init_failed = 0;
  for (int t = 0; t < T; ++t) {
    ribp_thread_range(N, t, T, &ctx.threads[t].begin, &ctx.threads[t].end);
    uint64_t rows_t = (uint64_t) (ctx.threads[t].end - ctx.threads[t].begin);
    init_failed |= ribp_table_init(&ctx.threads[t].tbl, ribp_slots_k(rows_t), rows_t / 16);
  }
  if (init_failed) {
    ribp_error(&ctx, "Unable to allocate hash tables."); // # nocov
  }

#if defined _OPENMP && _OPENMP >= 201511
  if (T > 1) {
#pragma omp parallel num_threads(T)
    {
      int t = omp_get_thread_num();
      int team_size = omp_get_num_threads();
#pragma omp single
      team = team_size;
      if (t < T) {
        ribp_thread * th = &ctx.threads[t];
        if (team_size != T) {
          // Fewer threads than requested: recompute contiguous ranges.
          ribp_thread_range(N, t, team_size, &th->begin, &th->end);
        }
        ribp_hash_rows(ctx.cols, ncol, na_is, th, outu);
      }
    }
  } else {
    ribp_hash_rows(ctx.cols, ncol, na_is, &ctx.threads[0], outu);
  }
#else
  ribp_hash_rows(ctx.cols, ncol, na_is, &ctx.threads[0], outu);
#endif
  if (team < T) {
    // Threads beyond the team did no work; drop them.
    for (int t = team; t < T; ++t) {
      ribp_table_free(&ctx.threads[t].tbl);
    }
    T = team;
    ctx.nthreads = T;
  }
  for (int t = 0; t < T; ++t) {
    if (ctx.threads[t].failed) {
      ribp_error(&ctx, "Unable to allocate memory while hashing rows (or more than 2^32 - 1 distinct patterns in one thread)."); // # nocov
    }
  }

  // ---- Pass 2: merge per-thread tables into partitions ----------------------
  ctx.parts = calloc(T, sizeof(ribp_table));
  ctx.remap = calloc(T, sizeof(uint64_t *));
  ctx.offsets = calloc(T + 1, sizeof(uint64_t));
  if (ctx.parts == NULL || ctx.remap == NULL || ctx.offsets == NULL) {
    ribp_error(&ctx, "Unable to allocate merge state."); // # nocov
  }
  ctx.nparts = T;
  {
    uint64_t total_local = 0;
    for (int t = 0; t < T; ++t) {
      total_local += ctx.threads[t].tbl.nused;
      ctx.remap[t] = malloc(sizeof(uint64_t) * (ctx.threads[t].tbl.nused ? ctx.threads[t].tbl.nused : 1));
      if (ctx.remap[t] == NULL) {
        ribp_error(&ctx, "Unable to allocate remap arrays."); // # nocov
      }
    }
    uint64_t expected = total_local / T + total_local / (4 * T) + 1;
    int part_k = ribp_slots_k(expected);
    int part_failed = 0;
    for (int q = 0; q < T; ++q) {
      part_failed |= ribp_table_init(&ctx.parts[q], part_k, expected / 4);
    }
    if (part_failed) {
      ribp_error(&ctx, "Unable to allocate partition tables."); // # nocov
    }
  }
  // Bucket each thread's entries by partition (top bits of h2, independent of
  // the h1 bits used for slots): copy them into partition order so that the
  // merge reads each entry once, sequentially. The local slot arrays are no
  // longer needed and are freed first.
  ctx.order = calloc(T, sizeof(uint32_t *));
  ctx.sorted = calloc(T, sizeof(ribp_entry *));
  ctx.bucket_off = calloc((size_t) T * (T + 1), sizeof(uint64_t));
  if (ctx.order == NULL || ctx.sorted == NULL || ctx.bucket_off == NULL) {
    ribp_error(&ctx, "Unable to allocate merge buckets."); // # nocov
  }
  for (int t = 0; t < T; ++t) {
    uint64_t nused = ctx.threads[t].tbl.nused;
    free(ctx.threads[t].tbl.slots);
    ctx.threads[t].tbl.slots = NULL;
    ctx.order[t] = malloc(sizeof(uint32_t) * (nused ? nused : 1));
    ctx.sorted[t] = malloc(sizeof(ribp_entry) * (nused ? nused : 1));
    if (ctx.order[t] == NULL || ctx.sorted[t] == NULL) {
      ribp_error(&ctx, "Unable to allocate merge buckets."); // # nocov
    }
  }
  const uint64_t Tq = (uint64_t) T;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(T) schedule(static, 1)
#endif
  for (int t = 0; t < T; ++t) {
    const ribp_table * lt = &ctx.threads[t].tbl;
    uint64_t * off = ctx.bucket_off + (size_t) t * (T + 1);
    uint32_t * order_t = ctx.order[t];
    ribp_entry * sorted_t = ctx.sorted[t];
    for (int q = 0; q <= T; ++q) {
      off[q] = 0;
    }
    for (uint64_t i = 0; i < lt->nused; ++i) {
      uint64_t part = ((lt->entries[i].h2 >> 32) * Tq) >> 32;
      off[part + 1]++;
    }
    for (int q = 0; q < T; ++q) {
      off[q + 1] += off[q];
    }
    for (uint64_t i = 0; i < lt->nused; ++i) {
      uint64_t part = ((lt->entries[i].h2 >> 32) * Tq) >> 32;
      uint64_t pos = off[part]++;
      sorted_t[pos] = lt->entries[i];
      order_t[pos] = (uint32_t) i;
    }
    // off[q] now holds the end of bucket q; restore the starts.
    for (int q = T; q > 0; --q) {
      off[q] = off[q - 1];
    }
    off[0] = 0;
  }
  for (int t = 0; t < T; ++t) {
    ctx.threads[t].nlocal = ctx.threads[t].tbl.nused;
    ribp_table_free(&ctx.threads[t].tbl);
  }
  int merge_failed = 0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(T) schedule(static, 1) reduction(| : merge_failed)
#endif
  for (int q = 0; q < T; ++q) {
    ribp_table * pt = &ctx.parts[q];
    for (int tt = 0; tt < T; ++tt) {
      int t = (q + tt) % T;
      const uint64_t * off = ctx.bucket_off + (size_t) t * (T + 1);
      const ribp_entry * sorted_t = ctx.sorted[t];
      const uint32_t * order_t = ctx.order[t];
      uint64_t * remap_t = ctx.remap[t];
      const uint64_t kend = off[q + 1];
      for (uint64_t k = off[q]; k < kend; ++k) {
        if (k + RIBP_PREFETCH_AHEAD < kend) {
          RIBP_PREFETCH(&pt->slots[sorted_t[k + RIBP_PREFETCH_AHEAD].h1 >> (64 - pt->k)]);
        }
        const ribp_entry * E = &sorted_t[k];
        uint32_t id = ribp_insert(pt, E->h1, E->h2, E->first_row, E->count);
        if (id == UINT32_MAX) {
          merge_failed = 1; // # nocov
          break;            // # nocov
        }
        remap_t[order_t[k]] = ((uint64_t) q << 32) | id;
      }
    }
  }
  if (merge_failed) {
    ribp_error(&ctx, "Unable to allocate memory while merging patterns."); // # nocov
  }
  uint64_t G = 0;
  for (int q = 0; q < T; ++q) {
    ctx.offsets[q] = G;
    G += ctx.parts[q].nused;
  }
  ctx.offsets[T] = G;
  // Sorted copies are no longer needed.
  for (int t = 0; t < T; ++t) {
    free(ctx.sorted[t]);
    ctx.sorted[t] = NULL;
    free(ctx.order[t]);
    ctx.order[t] = NULL;
  }

  // ---- Rank groups by (count desc, first_row asc) ----------------------------
  ctx.final_id = malloc(sizeof(int) * G);
  if (ctx.final_id == NULL) {
    ribp_error(&ctx, "Unable to allocate group ids."); // # nocov
  }
  uint64_t max_count = 0;
  for (int q = 0; q < T; ++q) {
    for (uint64_t i = 0; i < ctx.parts[q].nused; ++i) {
      uint64_t c = ctx.parts[q].entries[i].count;
      max_count = c > max_count ? c : max_count;
    }
  }
  if (max_count <= UINT32_MAX && (uint64_t) N <= UINT32_MAX && G <= UINT32_MAX) {
    ctx.keys = malloc(sizeof(uint64_t) * G);
    ctx.keys2 = malloc(sizeof(uint64_t) * G);
    ctx.vals = malloc(sizeof(uint32_t) * G);
    ctx.vals2 = malloc(sizeof(uint32_t) * G);
    if (ctx.keys == NULL || ctx.keys2 == NULL || ctx.vals == NULL || ctx.vals2 == NULL) {
      ribp_error(&ctx, "Unable to allocate sort buffers."); // # nocov
    }
    for (int q = 0; q < T; ++q) {
      uint64_t off = ctx.offsets[q];
      for (uint64_t i = 0; i < ctx.parts[q].nused; ++i) {
        const ribp_entry * E = &ctx.parts[q].entries[i];
        ctx.keys[off + i] = ((uint64_t) (UINT32_MAX - (uint32_t) E->count) << 32) | (uint64_t) (uint32_t) E->first_row;
        ctx.vals[off + i] = (uint32_t) (off + i);
      }
    }
    // LSD radix sort; count the passes actually performed so we know which
    // buffer holds the result.
    uint64_t * keys = ctx.keys, * keys2 = ctx.keys2;
    uint32_t * vals = ctx.vals, * vals2 = ctx.vals2;
    {
      static uint64_t hist[65536];
      for (int pass = 0; pass < 4; ++pass) {
        int shift = 16 * pass;
        memset(hist, 0, sizeof(hist));
        for (uint64_t i = 0; i < G; ++i) {
          hist[(keys[i] >> shift) & 0xffff]++;
        }
        int nonempty = 0;
        for (int d = 0; d < 65536 && nonempty < 2; ++d) {
          nonempty += hist[d] != 0;
        }
        if (nonempty < 2) {
          continue;
        }
        uint64_t sum = 0;
        for (int d = 0; d < 65536; ++d) {
          uint64_t c = hist[d];
          hist[d] = sum;
          sum += c;
        }
        for (uint64_t i = 0; i < G; ++i) {
          uint64_t d = (keys[i] >> shift) & 0xffff;
          uint64_t pos = hist[d]++;
          keys2[pos] = keys[i];
          vals2[pos] = vals[i];
        }
        uint64_t * tk = keys; keys = keys2; keys2 = tk;
        uint32_t * tv = vals; vals = vals2; vals2 = tv;
      }
    }
    for (uint64_t p = 0; p < G; ++p) {
      ctx.final_id[vals[p]] = p < max_patterns ? (int) (p + 1) : NA_INTEGER;
    }
    free(ctx.keys); free(ctx.keys2); free(ctx.vals); free(ctx.vals2);
    ctx.keys = NULL; ctx.keys2 = NULL; ctx.vals = NULL; ctx.vals2 = NULL;
  } else {
    // # nocov start
    ribp_group * groups = malloc(sizeof(ribp_group) * G);
    if (groups == NULL) {
      ribp_error(&ctx, "Unable to allocate sort buffers.");
    }
    for (int q = 0; q < T; ++q) {
      uint64_t off = ctx.offsets[q];
      for (uint64_t i = 0; i < ctx.parts[q].nused; ++i) {
        const ribp_entry * E = &ctx.parts[q].entries[i];
        groups[off + i].count = E->count;
        groups[off + i].first = E->first_row;
        groups[off + i].gid = off + i;
      }
    }
    qsort(groups, G, sizeof(ribp_group), ribp_group_cmp);
    for (uint64_t p = 0; p < G; ++p) {
      ctx.final_id[groups[p].gid] = p < max_patterns ? (int) (p + 1) : NA_INTEGER;
    }
    free(groups);
    // # nocov end
  }

  // ---- Remap local ids to final ids ---------------------------------------
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(T) schedule(static, 1)
#endif
  for (int t = 0; t < T; ++t) {
    uint64_t * remap_t = ctx.remap[t];
    R_xlen_t begin = ctx.threads[t].begin;
    R_xlen_t end = ctx.threads[t].end;
    uint64_t nlocal = ctx.threads[t].nlocal;
    // Convert (q, id_in_q) to the final id in place, then rewrite this
    // thread's rows.
    for (uint64_t l = 0; l < nlocal; ++l) {
      uint64_t qid = remap_t[l];
      uint64_t q = qid >> 32;
      uint64_t id = qid & 0xffffffffu;
      remap_t[l] = (uint64_t) (uint32_t) ctx.final_id[ctx.offsets[q] + id];
    }
    for (R_xlen_t i = begin; i < end; ++i) {
      ansp[i] = (int) (uint32_t) remap_t[outu[i]];
    }
  }

  ribp_ctx_free(&ctx);
  UNPROTECT(1);
  return ans;
}
