#include "cpphutils.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(rng = false)]]
LogicalVector do_and3_par(IntegerVector x, int ox, int x1, int x2,
                          IntegerVector y, int oy, int y1, int y2,
                          IntegerVector z, int oz, int z1, int z2,
                          LogicalVector A,
                          LogicalVector B,
                          LogicalVector C,
                          CharacterVector nom,
                          int nThread = 1) {
  int nx = x.length();
  int nA = A.length();
  int n = (nx > nA) ? nx : nA;
  bool useX = x.length() == n;
  bool useY = y.length() == n;
  bool useZ = z.length() == n;

  // Which variables are bare logicals
  bool A_lgl = A.length() == n;
  bool B_lgl = B.length() == n;
  bool C_lgl = C.length() == n;

  // Is the 1st, 2nd, 3rd expression usable or should we just set it to false?
  bool e1 = useX || A_lgl;
  bool e2 = useY || B_lgl;
  bool e3 = useZ || C_lgl;

  // Are the expressions preceded by `!` -- i.e the opposite
  bool A_opposite = A_lgl && ox == 1;
  bool B_opposite = B_lgl && oy == 1;
  bool C_opposite = C_lgl && oz == 1;

  if (useX && A_lgl) {
    stop("Internal error: useX && A_lgl");
  }
  if (useY && B_lgl) {
    stop("Internal error: useY && B_lgl");
  }
  if (useZ && C_lgl) {
    stop("Internal error: useZ && C_lgl");
  }

  if (!e1 && !e2 && !e3) {
    return LogicalVector(n);
  }

  LogicalVector out = no_init(n);

  if (useX && useY && useZ) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        single_ox_x1_x2(x[i], ox, x1, x2) &&
        single_ox_x1_x2(y[i], oy, y1, y2) &&
        single_ox_x1_x2(z[i], oz, z1, z2);
    }
    return out;
  }

  if (useX && useY && C_opposite) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        single_ox_x1_x2(x[i], ox, x1, x2) &&
        single_ox_x1_x2(y[i], oy, y1, y2) &&
        !C[i];
    }
    return out;
  }

  if (useX && useY && C_lgl) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        single_ox_x1_x2(x[i], ox, x1, x2) &&
        single_ox_x1_x2(y[i], oy, y1, y2) &&
        C[i];
    }
    return out;
  }

  // e3 = false
  if (useX && useY && !e3) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        single_ox_x1_x2(x[i], ox, x1, x2) &&
        single_ox_x1_x2(y[i], oy, y1, y2);
    }
    return out;
  }

  // B_lgl (but B_opposite must always precede!)
  if (useX && B_opposite && useZ) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        single_ox_x1_x2(x[i], ox, x1, x2) &&
        !B[i] &&
        single_ox_x1_x2(z[i], oz, z1, z2);
    }
    return out;
  }

  if (useX && B_lgl && useZ) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        single_ox_x1_x2(x[i], ox, x1, x2) &&
        B[i] &&
        single_ox_x1_x2(z[i], oz, z1, z2);
    }
    return out;
  }

  if (useX && B_opposite && C_opposite) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        single_ox_x1_x2(x[i], ox, x1, x2) &&
        !B[i] &&
        !C[i];
    }
    return out;
  }

  if (useX && B_lgl && C_opposite) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        single_ox_x1_x2(x[i], ox, x1, x2) &&
        B[i] &&
        !C[i];
    }
    return out;
  }

  if (useX && B_opposite && C_lgl) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        single_ox_x1_x2(x[i], ox, x1, x2) &&
        !B[i] &&
        C[i];
    }
    return out;
  }

  if (useX && B_lgl && C_lgl) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        single_ox_x1_x2(x[i], ox, x1, x2) &&
        B[i] &&
        C[i];
    }
    return out;
  }

  if (useX && B_opposite && !e3) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        single_ox_x1_x2(x[i], ox, x1, x2) &&
        !B[i];
    }
    return out;
  }

  if (useX && B_lgl && !e3) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        single_ox_x1_x2(x[i], ox, x1, x2) &&
        B[i];
    }
    return out;
  }

  if (useX && !e2 && !e3) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        single_ox_x1_x2(x[i], ox, x1, x2);
    }
    return out;
  }
  // // A_opposite

  if (A_opposite && useY && useZ) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        single_ox_x1_x2(y[i], oy, y1, y2) &&
        single_ox_x1_x2(z[i], oz, z1, z2);
    }
    return out;
  }

  if (A_opposite && useY && C_opposite) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        single_ox_x1_x2(y[i], oy, y1, y2) &&
        !C[i];
    }
    return out;
  }

  if (A_opposite && useY && C_lgl) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        single_ox_x1_x2(y[i], oy, y1, y2) &&
        C[i];
    }
    return out;
  }

  // e3 = false
  if (A_opposite && useY && !e3) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        single_ox_x1_x2(y[i], oy, y1, y2);
    }
    return out;
  }

  // B_lgl
  if (A_opposite && B_opposite && useZ) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        !B[i] &&
        single_ox_x1_x2(z[i], oz, z1, z2);
    }
    return out;
  }

  if (A_opposite && B_lgl && useZ) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        B[i] &&
        single_ox_x1_x2(z[i], oz, z1, z2);
    }
    return out;
  }

  if (A_opposite && B_opposite && C_opposite) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        !B[i] &&
        !C[i];
    }
    return out;
  }

  if (A_opposite && B_lgl && C_opposite) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        B[i] &&
        !C[i];
    }
    return out;
  }

  if (A_opposite && B_opposite && C_lgl) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        !B[i] &&
        C[i];
    }
    return out;
  }

  if (A_opposite && B_lgl && C_lgl) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        B[i] &&
        C[i];
    }
    return out;
  }

  if (A_opposite && B_opposite && !e3) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        !B[i];
    }
    return out;
  }

  if (A_opposite && B_lgl && !e3) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        B[i];
    }
    return out;
  }

  if (A_opposite && !e2 && !e3) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i];
    }
    return out;
  }

  // // A_lgl

  if (A_lgl && useY && useZ) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        single_ox_x1_x2(y[i], oy, y1, y2) &&
        single_ox_x1_x2(z[i], oz, z1, z2);
    }
    return out;
  }

  if (A_lgl && useY && C_opposite) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        single_ox_x1_x2(y[i], oy, y1, y2) &&
        !C[i];
    }
    return out;
  }

  if (A_lgl && useY && C_lgl) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        single_ox_x1_x2(y[i], oy, y1, y2) &&
        C[i];
    }
    return out;
  }

  // e3 = false
  if (A_lgl && useY && !e3) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        single_ox_x1_x2(y[i], oy, y1, y2);
    }
    return out;
  }

  // B_lgl
  if (A_lgl && B_opposite && useZ) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        !B[i] &&
        single_ox_x1_x2(z[i], oz, z1, z2);
    }
    return out;
  }

  if (A_lgl && B_lgl && useZ) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        B[i] &&
        single_ox_x1_x2(z[i], oz, z1, z2);
    }
    return out;
  }

  if (A_lgl && B_opposite && C_opposite) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        !B[i] &&
        !C[i];
    }
    return out;
  }

  if (A_lgl && B_lgl && C_opposite) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        B[i] &&
        !C[i];
    }
    return out;
  }

  if (A_lgl && B_opposite && C_lgl) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        !B[i] &&
        C[i];
    }
    return out;
  }

  if (A_lgl && B_lgl && C_lgl) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        B[i] &&
        C[i];
    }
    return out;
  }

  if (A_lgl && B_opposite && !e3) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        !B[i];
    }
    return out;
  }

  if (A_lgl && B_lgl && !e3) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        B[i];
    }
    return out;
  }

  if (A_lgl && !e2 && !e3) {
#pragma omp parallel for num_threads(nThread)
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i];
    }
    return out;
  }

#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < n; ++i) {
    bool oi = false;

    // 1st expression
    if (e1) {
      oi = useX ? single_ox_x1_x2(x[i], ox, x1, x2) : (A_opposite ? !A[i] : A[i]);
      if (!oi) {
        out[i] = FALSE;
        continue;
      }
    }

    // 2nd expression
    // #nocov start
    if (e2) {
      oi = useY ? single_ox_x1_x2(y[i], oy, y1, y2) : (B_opposite ? !B[i] : B[i]);
      if (!oi) {
        out[i] = FALSE;
        continue;
      }
    }
    // #nocov end
    // 3rd expression
    if (e3) {
      oi = useZ ? single_ox_x1_x2(z[i], oz, z1, z2) : (C_opposite ? !C[i] : C[i]);
      if (!oi) {
        out[i] = FALSE;
        continue;
      }
    }

    out[i] = TRUE;

  }
  return out;
}

// [[Rcpp::export]]
R_xlen_t do_sum3s_par(IntegerVector x, int ox, int x1, int x2,
                      IntegerVector y, int oy, int y1, int y2,
                      IntegerVector z, int oz, int z1, int z2,
                      LogicalVector A,
                      LogicalVector B,
                      LogicalVector C,
                      int nThread = 1) {
  R_xlen_t nx = x.length();
  R_xlen_t nA = A.length();
  R_xlen_t n = (nx > nA) ? nx : nA;
  bool useX = x.length() == n;
  bool useY = y.length() == n;
  bool useZ = z.length() == n;

  // Which variables are bare logicals
  bool A_lgl = A.length() == n;
  bool B_lgl = B.length() == n;
  bool C_lgl = C.length() == n;

  // Is the 1st, 2nd, 3rd expression usable or should we just set it to false?
  bool e1 = useX || A_lgl;
  bool e2 = useY || B_lgl;
  bool e3 = useZ || C_lgl;

  // Are the expressions preceded by `!` -- i.e the opposite
  bool A_opposite = A_lgl && ox == 1;
  bool B_opposite = B_lgl && oy == 1;
  bool C_opposite = C_lgl && oz == 1;

  if (useX && A_lgl) {
    stop("Internal error: useX && A_lgl");
  }
  if (useY && B_lgl) {
    stop("Internal error: useY && B_lgl");
  }
  if (useZ && C_lgl) {
    stop("Internal error: useZ && C_lgl");
  }

  R_xlen_t out = 0;

#pragma omp parallel for num_threads(nThread) reduction(+ : out)
  for (R_xlen_t i = 0; i < n; ++i) {
    bool oi = false;

    // 1st expression
    if (e1) {
      oi = useX ? single_ox_x1_x2(x[i], ox, x1, x2) : (A_opposite ? !A[i] : A[i]);
      if (!oi) {
        continue;
      }
    }

    // 2nd expression
    // #nocov start
    if (e2) {
      oi = useY ? single_ox_x1_x2(y[i], oy, y1, y2) : (B_opposite ? !B[i] : B[i]);
      if (!oi) {
        continue;
      }
    }
    // #nocov end
    // 3rd expression
    if (e3) {
      oi = useZ ? single_ox_x1_x2(z[i], oz, z1, z2) : (C_opposite ? !C[i] : C[i]);
      if (!oi) {
        continue;
      }
    }
    out += 1;
  }
  // if (out < INT_MAX) {
  //   return ScalarInteger(out);
  // }
  // return ScalarReal(out);
  return out;
}

#include "nmmintrin.h"
#include "immintrin.h"

// __m128i _mm_min_epi32 (__m128i a, __m128i b);

// [[Rcpp::export]]
int simd_sum(IntegerVector x, int a, IntegerVector y, int b) {
  // union { __m128i oz4; __m128i ow4; };
  // oz4 = ow4 = _mm_setzero_ps();
  return x[0];
}

