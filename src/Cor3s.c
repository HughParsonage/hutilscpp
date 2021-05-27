#include "hutilscpp.h"


SEXP Cor3_par(SEXP xx, SEXP oxx, SEXP x11, SEXP x22,
              SEXP yy, SEXP oyy, SEXP y11, SEXP y22,
              SEXP zz, SEXP ozz, SEXP z11, SEXP z22,
              SEXP AA,
              SEXP BB,
              SEXP CC,
              SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  const int * x = INTEGER(xx);
  const int * y = INTEGER(yy);
  const int * z = INTEGER(zz);
  const int ox = asInteger(oxx);
  const int oy = asInteger(oyy);
  const int oz = asInteger(ozz);
  const int x1 = asInteger(x11);
  const int y1 = asInteger(y11);
  const int z1 = asInteger(z11);
  const int x2 = asInteger(x22);
  const int y2 = asInteger(y22);
  const int z2 = asInteger(z22);

  const int * A = LOGICAL(AA);
  const int * B = LOGICAL(BB);
  const int * C = LOGICAL(CC);

  R_xlen_t nx = xlength(xx);
  R_xlen_t nA = xlength(AA);
  R_xlen_t n = (nx > nA) ? nx : nA;
  bool useX = xlength(xx) == n;
  bool useY = xlength(yy) == n;
  bool useZ = xlength(zz) == n;

  // Which variables are bare logicals
  bool A_lgl = xlength(AA) == n;
  bool B_lgl = xlength(BB) == n;
  bool C_lgl = xlength(CC) == n;

  // Is the 1st, 2nd, 3rd expression usable or should we just set it to false?
  bool e1 = useX || A_lgl;
  bool e2 = useY || B_lgl;
  bool e3 = useZ || C_lgl;

  // Are the expressions preceded by `!` -- i.e the opposite
  bool A_opposite = A_lgl && ox == 1;
  bool B_opposite = B_lgl && oy == 1;
  bool C_opposite = C_lgl && oz == 1;

  if (useX && A_lgl) {
    error("Internal error: useX && A_lgl"); // # nocov
  }
  if (useY && B_lgl) {
    error("Internal error: useY && B_lgl"); // # nocov
  }
  if (useZ && C_lgl) {
    error("Internal error: useZ && C_lgl"); // # nocov
  }

  if (!e1 && !e2 && !e3) {
    return LogicalN(n); // # nocov
  }

  SEXP ans = PROTECT(allocVector(LGLSXP, n));
  int * out = LOGICAL(ans);

  if (useX && useY && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) ||
        isingle_ox_x1_x2(y[i], oy, y1, y2) ||
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (useX && useY && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) ||
        isingle_ox_x1_x2(y[i], oy, y1, y2) ||
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && useY && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) ||
        isingle_ox_x1_x2(y[i], oy, y1, y2) ||
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  // e3 = false
  if (useX && useY && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) ||
        isingle_ox_x1_x2(y[i], oy, y1, y2);
    }
    UNPROTECT(1); return ans;
  }

  // B_lgl (but B_opposite must always precede!)
  if (useX && B_opposite && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) ||
        !B[i] ||
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_lgl && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) ||
        B[i] ||
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_opposite && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) ||
        !B[i] ||
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_lgl && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) ||
        B[i] ||
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_opposite && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) ||
        !B[i] ||
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_lgl && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) ||
        B[i] ||
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_opposite && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) ||
        !B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_lgl && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) ||
        B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && !e2 && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2);
    }
    UNPROTECT(1); return ans;
  }
  // // A_opposite

  if (A_opposite && useY && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] ||
        isingle_ox_x1_x2(y[i], oy, y1, y2) ||
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && useY && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] ||
        isingle_ox_x1_x2(y[i], oy, y1, y2) ||
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && useY && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] ||
        isingle_ox_x1_x2(y[i], oy, y1, y2) ||
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  // e3 = false
  if (A_opposite && useY && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] ||
        isingle_ox_x1_x2(y[i], oy, y1, y2);
    }
    UNPROTECT(1); return ans;
  }

  // B_lgl
  if (A_opposite && B_opposite && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] ||
        !B[i] ||
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_lgl && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] ||
        B[i] ||
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_opposite && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] ||
        !B[i] ||
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_lgl && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] ||
        B[i] ||
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_opposite && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] ||
        !B[i] ||
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_lgl && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] ||
        B[i] ||
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_opposite && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] ||
        !B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_lgl && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i] ||
        B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && !e2 && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        !A[i];
    }
    UNPROTECT(1); return ans;
  }

  // // A_lgl

  if (A_lgl && useY && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] ||
        isingle_ox_x1_x2(y[i], oy, y1, y2) ||
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && useY && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] ||
        isingle_ox_x1_x2(y[i], oy, y1, y2) ||
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && useY && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] ||
        isingle_ox_x1_x2(y[i], oy, y1, y2) ||
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  // e3 = false
  if (A_lgl && useY && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] ||
        isingle_ox_x1_x2(y[i], oy, y1, y2);
    }
    UNPROTECT(1); return ans;
  }

  // B_lgl
  if (A_lgl && B_opposite && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] ||
        !B[i] ||
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_lgl && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] ||
        B[i] ||
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_opposite && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] ||
        !B[i] ||
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_lgl && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] ||
        B[i] ||
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_opposite && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] ||
        !B[i] ||
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_lgl && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] ||
        B[i] ||
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_opposite && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] ||
        !B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_lgl && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i] ||
        B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && !e2 && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (int i = 0; i < n; ++i) {
      out[i] =
        A[i];
    }
    UNPROTECT(1); return ans;
  }

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (int i = 0; i < n; ++i) {
    bool oi = false;

    // 1st expression
    if (e1) {
      oi = useX ? isingle_ox_x1_x2(x[i], ox, x1, x2) : (A_opposite ? !A[i] : A[i]);
      if (oi) {
        out[i] = oi;
        continue;
      }
    }

    // 2nd expression
    // #nocov start
    if (e2) {
      oi = useY ? isingle_ox_x1_x2(y[i], oy, y1, y2) : (B_opposite ? !B[i] : B[i]);
      if (oi) {
        out[i] = TRUE;
        continue;
      }
    }
    // #nocov end
    // 3rd expression
    if (e3) {
      oi = useZ ? isingle_ox_x1_x2(z[i], oz, z1, z2) : (C_opposite ? !C[i] : C[i]);
      if (oi) {
        out[i] = TRUE;
        continue;
      }
    }

    out[i] = FALSE;

  }
  UNPROTECT(1);
  return ans;
}










