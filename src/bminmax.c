#include "hutilscpp.h"

#include <float.h>
#include <string.h>

#ifndef __has_attribute
#define __has_attribute(x) 0
#endif
#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

// Compile the specialized functions only when the architecture and compiler
// can isolate them from the baseline binary. Runtime CPU/OS support is checked
// separately before any specialized function is called.
#if !defined(HUTILSCPP_DISABLE_AVX512) && \
    (defined(__x86_64__) || defined(__i386__)) && \
    ((defined(__GNUC__) && !defined(__clang__) && __GNUC__ >= 6) || \
     (defined(__clang__) && __has_attribute(target) && \
      __has_builtin(__builtin_cpu_supports)))
#include <immintrin.h>
#define HUTILSCPP_CAN_COMPILE_AVX512 1
#else
#define HUTILSCPP_CAN_COMPILE_AVX512 0
#endif

// Parallel execution has a fixed team-formation cost. These conservative
// gates were calibrated for the point where it starts beating the serial
// AVX-512 path. Keeping about 512 KiB of input per worker also prevents
// oversized teams while allowing the requested maximum on long vectors.
#define BMINMAX_DOUBLE_PARALLEL_MIN_N 250000
#define BMINMAX_INTEGER_PARALLEL_MIN_N 500000
#define BMINMAX_DOUBLE_MIN_CHUNK 65536
#define BMINMAX_INTEGER_MIN_CHUNK 131072

typedef struct {
  double xmin;
  double xmax;
} bminmax_bounds;

static bool bminmax_double_portable(const double * x, R_xlen_t n, bminmax_bounds * bounds) {
  double xmin = DBL_MAX;
  double xmax = 0.0;

  for (R_xlen_t i = 0; i < n; ++i) {
    const double xi = x[i];
    if (!(xi > 0.0 && xi <= DBL_MAX)) {
      return false;
    }
    xmin = xi < xmin ? xi : xmin;
    xmax = xi > xmax ? xi : xmax;
  }

  bounds->xmin = xmin;
  bounds->xmax = xmax;
  return true;
}

static bool bminmax_integer_portable(const int * x, R_xlen_t n, bminmax_bounds * bounds) {
  int xmin = INT_MAX;
  int xmax = 0;

  for (R_xlen_t i = 0; i < n; ++i) {
    const int xi = x[i];
    if (xi <= 0) {
      return false;
    }
    xmin = xi < xmin ? xi : xmin;
    xmax = xi > xmax ? xi : xmax;
  }

  bounds->xmin = (double)xmin;
  bounds->xmax = (double)xmax;
  return true;
}

#if HUTILSCPP_CAN_COMPILE_AVX512

__attribute__((target("avx512f")))
static bool bminmax_double_avx512(const double * x, R_xlen_t n, bminmax_bounds * bounds) {
  const __m512d zero = _mm512_setzero_pd();
  const __m512d largest = _mm512_set1_pd(DBL_MAX);
  __m512d min0 = largest;
  __m512d min1 = largest;
  __m512d min2 = largest;
  __m512d min3 = largest;
  __m512d max0 = zero;
  __m512d max1 = zero;
  __m512d max2 = zero;
  __m512d max3 = zero;
  R_xlen_t i = 0;

  for (; i + 31 < n; i += 32) {
    const __m512d x0 = _mm512_loadu_pd(x + i);
    const __m512d x1 = _mm512_loadu_pd(x + i + 8);
    const __m512d x2 = _mm512_loadu_pd(x + i + 16);
    const __m512d x3 = _mm512_loadu_pd(x + i + 24);
    const __mmask8 valid0 = _mm512_cmp_pd_mask(x0, zero, _CMP_GT_OQ) & _mm512_cmp_pd_mask(x0, largest, _CMP_LE_OQ);
    const __mmask8 valid1 = _mm512_cmp_pd_mask(x1, zero, _CMP_GT_OQ) & _mm512_cmp_pd_mask(x1, largest, _CMP_LE_OQ);
    const __mmask8 valid2 = _mm512_cmp_pd_mask(x2, zero, _CMP_GT_OQ) & _mm512_cmp_pd_mask(x2, largest, _CMP_LE_OQ);
    const __mmask8 valid3 = _mm512_cmp_pd_mask(x3, zero, _CMP_GT_OQ) & _mm512_cmp_pd_mask(x3, largest, _CMP_LE_OQ);
    if ((valid0 & valid1 & valid2 & valid3) != (__mmask8)0xff) {
      return false;
    }
    min0 = _mm512_min_pd(min0, x0);
    min1 = _mm512_min_pd(min1, x1);
    min2 = _mm512_min_pd(min2, x2);
    min3 = _mm512_min_pd(min3, x3);
    max0 = _mm512_max_pd(max0, x0);
    max1 = _mm512_max_pd(max1, x1);
    max2 = _mm512_max_pd(max2, x2);
    max3 = _mm512_max_pd(max3, x3);
  }

  min0 = _mm512_min_pd(_mm512_min_pd(min0, min1), _mm512_min_pd(min2, min3));
  max0 = _mm512_max_pd(_mm512_max_pd(max0, max1), _mm512_max_pd(max2, max3));

  for (; i + 7 < n; i += 8) {
    const __m512d xi = _mm512_loadu_pd(x + i);
    const __mmask8 valid = _mm512_cmp_pd_mask(xi, zero, _CMP_GT_OQ) & _mm512_cmp_pd_mask(xi, largest, _CMP_LE_OQ);
    if (valid != (__mmask8)0xff) {
      return false;
    }
    min0 = _mm512_min_pd(min0, xi);
    max0 = _mm512_max_pd(max0, xi);
  }

  double xmin = _mm512_reduce_min_pd(min0);
  double xmax = _mm512_reduce_max_pd(max0);

  for (; i < n; ++i) {
    const double xi = x[i];
    if (!(xi > 0.0 && xi <= DBL_MAX)) {
      return false;
    }
    xmin = xi < xmin ? xi : xmin;
    xmax = xi > xmax ? xi : xmax;
  }

  bounds->xmin = xmin;
  bounds->xmax = xmax;
  return true;
}

__attribute__((target("avx512f")))
static bool bminmax_integer_avx512(const int * x, R_xlen_t n, bminmax_bounds * bounds) {
  const __m512i zero = _mm512_setzero_si512();
  __m512i min0 = _mm512_set1_epi32(INT_MAX);
  __m512i min1 = min0;
  __m512i min2 = min0;
  __m512i min3 = min0;
  __m512i max0 = zero;
  __m512i max1 = zero;
  __m512i max2 = zero;
  __m512i max3 = zero;
  R_xlen_t i = 0;

  for (; i + 63 < n; i += 64) {
    const __m512i x0 = _mm512_loadu_si512((const void *)(x + i));
    const __m512i x1 = _mm512_loadu_si512((const void *)(x + i + 16));
    const __m512i x2 = _mm512_loadu_si512((const void *)(x + i + 32));
    const __m512i x3 = _mm512_loadu_si512((const void *)(x + i + 48));
    const __mmask16 valid0 = _mm512_cmp_epi32_mask(x0, zero, _MM_CMPINT_GT);
    const __mmask16 valid1 = _mm512_cmp_epi32_mask(x1, zero, _MM_CMPINT_GT);
    const __mmask16 valid2 = _mm512_cmp_epi32_mask(x2, zero, _MM_CMPINT_GT);
    const __mmask16 valid3 = _mm512_cmp_epi32_mask(x3, zero, _MM_CMPINT_GT);
    if ((valid0 & valid1 & valid2 & valid3) != (__mmask16)0xffff) {
      return false;
    }
    min0 = _mm512_min_epi32(min0, x0);
    min1 = _mm512_min_epi32(min1, x1);
    min2 = _mm512_min_epi32(min2, x2);
    min3 = _mm512_min_epi32(min3, x3);
    max0 = _mm512_max_epi32(max0, x0);
    max1 = _mm512_max_epi32(max1, x1);
    max2 = _mm512_max_epi32(max2, x2);
    max3 = _mm512_max_epi32(max3, x3);
  }

  min0 = _mm512_min_epi32(_mm512_min_epi32(min0, min1), _mm512_min_epi32(min2, min3));
  max0 = _mm512_max_epi32(_mm512_max_epi32(max0, max1), _mm512_max_epi32(max2, max3));

  for (; i + 15 < n; i += 16) {
    const __m512i xi = _mm512_loadu_si512((const void *)(x + i));
    if (_mm512_cmp_epi32_mask(xi, zero, _MM_CMPINT_GT) != (__mmask16)0xffff) {
      return false;
    }
    min0 = _mm512_min_epi32(min0, xi);
    max0 = _mm512_max_epi32(max0, xi);
  }

  int xmin = _mm512_reduce_min_epi32(min0);
  int xmax = _mm512_reduce_max_epi32(max0);

  for (; i < n; ++i) {
    const int xi = x[i];
    if (xi <= 0) {
      return false;
    }
    xmin = xi < xmin ? xi : xmin;
    xmax = xi > xmax ? xi : xmax;
  }

  bounds->xmin = (double)xmin;
  bounds->xmax = (double)xmax;
  return true;
}

static bool bminmax_has_avx512(void) {
  static int available = -1;
  if (available < 0) {
    __builtin_cpu_init();
    available = __builtin_cpu_supports("avx512f") ? 1 : 0;
  }
  return available == 1;
}

#else

static bool bminmax_has_avx512(void) {
  return false;
}

#endif

static int bminmax_effective_threads(R_xlen_t n, int requested, R_xlen_t parallel_min_n, R_xlen_t min_chunk) {
#ifdef _OPENMP
  if (requested <= 1 || n < parallel_min_n) {
    return 1;
  }
  const R_xlen_t useful_threads = n / min_chunk;
  if (useful_threads < 2) {
    return 1;
  }
  return useful_threads < requested ? (int)useful_threads : requested;
#else
  (void)n;
  (void)requested;
  (void)parallel_min_n;
  (void)min_chunk;
  return 1;
#endif
}

#ifdef _OPENMP

static void bminmax_thread_range(R_xlen_t n, int thread, int team_size, R_xlen_t * begin, R_xlen_t * length) {
  const R_xlen_t chunk = n / team_size;
  const R_xlen_t remainder = n % team_size;
  const R_xlen_t extra_before = thread < remainder ? thread : remainder;
  *begin = thread * chunk + extra_before;
  *length = chunk + (thread < remainder);
}

static bool bminmax_double_parallel(const double * x, R_xlen_t n, int n_thread, bool use_avx512, bminmax_bounds * bounds) {
  double xmin = DBL_MAX;
  double xmax = 0.0;
  int valid = 1;

#pragma omp parallel num_threads(n_thread) reduction(min : xmin) reduction(max : xmax) reduction(&& : valid)
  {
    R_xlen_t begin;
    R_xlen_t length;
    bminmax_bounds local;
    const int thread = omp_get_thread_num();
    const int team_size = omp_get_num_threads();
    bminmax_thread_range(n, thread, team_size, &begin, &length);

    bool local_valid;
#if HUTILSCPP_CAN_COMPILE_AVX512
    if (use_avx512) {
      local_valid = bminmax_double_avx512(x + begin, length, &local);
    } else {
      local_valid = bminmax_double_portable(x + begin, length, &local);
    }
#else
    (void)use_avx512;
    local_valid = bminmax_double_portable(x + begin, length, &local);
#endif
    if (local_valid) {
      xmin = local.xmin < xmin ? local.xmin : xmin;
      xmax = local.xmax > xmax ? local.xmax : xmax;
    } else {
      valid = 0;
    }
  }

  bounds->xmin = xmin;
  bounds->xmax = xmax;
  return valid != 0;
}

static bool bminmax_integer_parallel(const int * x, R_xlen_t n, int n_thread, bool use_avx512, bminmax_bounds * bounds) {
  int xmin = INT_MAX;
  int xmax = 0;
  int valid = 1;

#pragma omp parallel num_threads(n_thread) reduction(min : xmin) reduction(max : xmax) reduction(&& : valid)
  {
    R_xlen_t begin;
    R_xlen_t length;
    bminmax_bounds local;
    const int thread = omp_get_thread_num();
    const int team_size = omp_get_num_threads();
    bminmax_thread_range(n, thread, team_size, &begin, &length);

    bool local_valid;
#if HUTILSCPP_CAN_COMPILE_AVX512
    if (use_avx512) {
      local_valid = bminmax_integer_avx512(x + begin, length, &local);
    } else {
      local_valid = bminmax_integer_portable(x + begin, length, &local);
    }
#else
    (void)use_avx512;
    local_valid = bminmax_integer_portable(x + begin, length, &local);
#endif
    if (local_valid) {
      const int local_min = (int)local.xmin;
      const int local_max = (int)local.xmax;
      xmin = local_min < xmin ? local_min : xmin;
      xmax = local_max > xmax ? local_max : xmax;
    } else {
      valid = 0;
    }
  }

  bounds->xmin = (double)xmin;
  bounds->xmax = (double)xmax;
  return valid != 0;
}

#endif

static double bminmax_floor_power(double x) {
  uint64_t bits;
  memcpy(&bits, &x, sizeof(bits));
  const uint64_t exponent = bits & UINT64_C(0x7ff0000000000000);

  if (exponent != 0) {
    memcpy(&x, &exponent, sizeof(x));
    return x;
  }

  int binary_exponent;
  (void)frexp(x, &binary_exponent);
  return ldexp(1.0, binary_exponent - 1);
}

static double bminmax_ceiling_power(double x) {
  uint64_t bits;
  memcpy(&bits, &x, sizeof(bits));
  const uint64_t exponent = bits & UINT64_C(0x7ff0000000000000);
  const uint64_t fraction = bits & UINT64_C(0x000fffffffffffff);

  if (exponent != 0) {
    if (fraction == 0) {
      return x;
    }
    const uint64_t ceiling = exponent + UINT64_C(0x0010000000000000);
    memcpy(&x, &ceiling, sizeof(x));
    return x;
  }

  int binary_exponent;
  const double fraction_value = frexp(x, &binary_exponent);
  if (fraction_value == 0.5) {
    return x;
  }
  return ldexp(1.0, binary_exponent);
}

static SEXP bminmax_result(const bminmax_bounds * bounds) {
  SEXP ans = PROTECT(allocVector(REALSXP, 2));
  REAL(ans)[0] = bminmax_floor_power(bounds->xmin);
  REAL(ans)[1] = bminmax_ceiling_power(bounds->xmax);
  UNPROTECT(1);
  return ans;
}

static SEXP bminmax_impl(SEXP x, bool allow_avx512, int requested_threads) {
#if !HUTILSCPP_CAN_COMPILE_AVX512
  (void)allow_avx512;
#endif
  const bool x_is_real = isReal(x);
  const bool x_is_integer = isInteger(x);
  if (!x_is_real && !x_is_integer) {
    error("`x` was type '%s' but must be numeric (integer or double).", type2char(TYPEOF(x)));
  }

  const R_xlen_t n = xlength(x);
  if (n == 0) {
    error("`x` must be non-empty.");
  }

  bminmax_bounds bounds;
  bool valid;
  if (x_is_real) {
#if HUTILSCPP_CAN_COMPILE_AVX512
    const bool use_avx512 = allow_avx512 && n >= 32 && bminmax_has_avx512();
#else
    const bool use_avx512 = false;
#endif
    const int n_thread = bminmax_effective_threads(n, requested_threads, BMINMAX_DOUBLE_PARALLEL_MIN_N, BMINMAX_DOUBLE_MIN_CHUNK);
#ifdef _OPENMP
    if (n_thread > 1) {
      valid = bminmax_double_parallel(REAL(x), n, n_thread, use_avx512, &bounds);
    } else
#endif
#if HUTILSCPP_CAN_COMPILE_AVX512
    if (use_avx512) {
      valid = bminmax_double_avx512(REAL(x), n, &bounds);
    } else {
      valid = bminmax_double_portable(REAL(x), n, &bounds);
    }
#else
    valid = bminmax_double_portable(REAL(x), n, &bounds);
#endif
  } else {
#if HUTILSCPP_CAN_COMPILE_AVX512
    const bool use_avx512 = allow_avx512 && n >= 64 && bminmax_has_avx512();
#else
    const bool use_avx512 = false;
#endif
    const int n_thread = bminmax_effective_threads(n, requested_threads, BMINMAX_INTEGER_PARALLEL_MIN_N, BMINMAX_INTEGER_MIN_CHUNK);
#ifdef _OPENMP
    if (n_thread > 1) {
      valid = bminmax_integer_parallel(INTEGER(x), n, n_thread, use_avx512, &bounds);
    } else
#endif
#if HUTILSCPP_CAN_COMPILE_AVX512
    if (use_avx512) {
      valid = bminmax_integer_avx512(INTEGER(x), n, &bounds);
    } else {
      valid = bminmax_integer_portable(INTEGER(x), n, &bounds);
    }
#else
    valid = bminmax_integer_portable(INTEGER(x), n, &bounds);
#endif
  }

  if (!valid) {
    error("`x` must contain only positive, finite, non-missing values.");
  }
  return bminmax_result(&bounds);
}

SEXP Cbminmax(SEXP x, SEXP nthreads) {
  return bminmax_impl(x, true, as_nThread(nthreads));
}

SEXP Cbminmax_portable(SEXP x) {
  return bminmax_impl(x, false, 1);
}

SEXP Cbminmax_has_avx512(void) {
  return ScalarLogical(bminmax_has_avx512());
}
