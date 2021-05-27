#include "hutilscpp.h"

SEXP Cand3s_par(SEXP xx, SEXP oxx, SEXP x11, SEXP x22,
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
    return LogicalN(n);  // # nocov
  }

  SEXP ans = PROTECT(allocVector(LGLSXP, n));
  int * out = LOGICAL(ans);

  if (useX && useY && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (useX && useY && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && useY && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  // e3 = false
  if (useX && useY && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        isingle_ox_x1_x2(y[i], oy, y1, y2);
    }
    UNPROTECT(1); return ans;
  }

  // B_lgl (but B_opposite must always precede!)
  if (useX && B_opposite && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        !B[i] &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_lgl && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        B[i] &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_opposite && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        !B[i] &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_lgl && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        B[i] &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_opposite && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        !B[i] &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_lgl && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        B[i] &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_opposite && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        !B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && B_lgl && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        isingle_ox_x1_x2(x[i], ox, x1, x2) &&
        B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (useX && !e2 && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
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
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && useY && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && useY && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  // e3 = false
  if (A_opposite && useY && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2);
    }
    UNPROTECT(1); return ans;
  }

  // B_lgl
  if (A_opposite && B_opposite && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        !B[i] &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_lgl && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        B[i] &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_opposite && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        !B[i] &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_lgl && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        B[i] &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_opposite && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        !B[i] &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_lgl && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        B[i] &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_opposite && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        !B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && B_lgl && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        !A[i] &&
        B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_opposite && !e2 && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
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
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && useY && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && useY && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2) &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  // e3 = false
  if (A_lgl && useY && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        isingle_ox_x1_x2(y[i], oy, y1, y2);
    }
    UNPROTECT(1); return ans;
  }

  // B_lgl
  if (A_lgl && B_opposite && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        !B[i] &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_lgl && useZ) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        B[i] &&
        isingle_ox_x1_x2(z[i], oz, z1, z2);
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_opposite && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        !B[i] &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_lgl && C_opposite) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        B[i] &&
        !C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_opposite && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        !B[i] &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_lgl && C_lgl) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        B[i] &&
        C[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_opposite && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        !B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && B_lgl && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i] &&
        B[i];
    }
    UNPROTECT(1); return ans;
  }

  if (A_lgl && !e2 && !e3) {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] =
        A[i];
    }
    UNPROTECT(1); return ans;
  }

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < n; ++i) {
    bool oi = false;

    // 1st expression
    if (e1) {
      oi = useX ? isingle_ox_x1_x2(x[i], ox, x1, x2) : (A_opposite ? !A[i] : A[i]);
      if (!oi) {
        out[i] = FALSE;
        continue;
      }
    }

    // 2nd expression
    // #nocov start
    if (e2) {
      oi = useY ? isingle_ox_x1_x2(y[i], oy, y1, y2) : (B_opposite ? !B[i] : B[i]);
      if (!oi) {
        out[i] = FALSE;
        continue;
      }
    }
    // #nocov end
    // 3rd expression
    if (e3) {
      oi = useZ ? isingle_ox_x1_x2(z[i], oz, z1, z2) : (C_opposite ? !C[i] : C[i]);
      if (!oi) {
        out[i] = FALSE;
        continue;
      }
    }

    out[i] = TRUE;

  }
  UNPROTECT(1);
  return ans;
}

SEXP Csum3s_par(SEXP xx, SEXP oxx, SEXP x11, SEXP x22,
                SEXP xxd, SEXP xd11, SEXP xd22,
                SEXP yy, SEXP oyy, SEXP y11, SEXP y22,
                SEXP yyd, SEXP yd11, SEXP yd22,
                SEXP zz, SEXP ozz, SEXP z11, SEXP z22,
                SEXP zzd, SEXP zd11, SEXP zd22,
                SEXP AA,
                SEXP BB,
                SEXP CC,
                SEXP Aampersand,
                SEXP nthreads) {


  // ampersand TRUE => sum_and3,  FALSE  => sum_or3
  const bool ampersand = asLogical(Aampersand);
  int nThread = as_nThread(nthreads);
  const int * x = INTEGER(xx);
  const int * y = INTEGER(yy);
  const int * z = INTEGER(zz);
  const double * xd = REAL(xxd);
  const double * yd = REAL(yyd);
  const double * zd = REAL(zzd);
  const int ox = asInteger(oxx);
  const int oy = asInteger(oyy);
  const int oz = asInteger(ozz);
  const int x1 = asInteger(x11);
  const int y1 = asInteger(y11);
  const int z1 = asInteger(z11);
  const double xd1 = asReal(xd11);
  const double yd1 = asReal(yd11);
  const double zd1 = asReal(zd11);
  const int x2 = asInteger(x22);
  const int y2 = asInteger(y22);
  const int z2 = asInteger(z22);
  const double xd2 = asReal(xd22);
  const double yd2 = asReal(yd22);
  const double zd2 = asReal(zd22);

  const int * A = LOGICAL(AA);
  const int * B = LOGICAL(BB);
  const int * C = LOGICAL(CC);

  R_xlen_t lengths[9] = {xlength(xx), xlength(xxd),
                         xlength(yy), xlength(yyd),
                         xlength(zz), xlength(zzd),
                         xlength(AA), xlength(BB), xlength(CC)};

  R_xlen_t n = xlength(xx);
  for (int i = 1; i < 9; ++i) {
    if (n < lengths[i]) {
      n = lengths[i];
    }
  }

  // 0 use none, 1 use int, 2 use double, 3 use logical, 4 use opposite
  const int Xcase = (xlength(xx) == n) + 2 * (xlength(xxd) == n) + 3 * (xlength(AA) == n && ox != OP_NE) + 4 * (xlength(AA) == n && ox == OP_NE);
  const int Ycase = (xlength(yy) == n) + 2 * (xlength(yyd) == n) + 3 * (xlength(BB) == n && oy != OP_NE) + 4 * (xlength(BB) == n && oy == OP_NE);
  const int Zcase = (xlength(zz) == n) + 2 * (xlength(zzd) == n) + 3 * (xlength(CC) == n && oz != OP_NE) + 4 * (xlength(CC) == n && oz == OP_NE);


  R_xlen_t out = 0;

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : out)
#endif
  for (R_xlen_t i = 0; i < n; ++i) {
    bool oi = Xcase == 0;

    // 1st expression
    switch(Xcase) {
    case 1:
      oi = isingle_ox_x1_x2(x[i], ox, x1, x2);
      break;
    case 2:
      oi = dsingle_ox_x1_x2(xd[i], ox, xd1, xd2);
      break;
    case 3:
      oi = A[i];
      break;
    case 4:
      oi = !A[i];
      break;
    }
    if (ampersand) {
      if (!oi) {
        continue;
      }
    } else {
      if (oi) {
        out += 1;
        continue;
      }
    }


    // 2nd expression

    switch(Ycase) {
    case 1:
      oi = isingle_ox_x1_x2(y[i], oy, y1, y2);
      break;
    case 2:
      oi = dsingle_ox_x1_x2(yd[i], oy, yd1, yd2);
      break;
    case 3:
      oi = B[i];
      break;
    case 4:
      oi = !B[i];
      break;
    }
    if (ampersand) {
      if (!oi) {
        continue;
      }
    } else {
      if (oi) {
        out += 1;
        continue;
      }
    }

    // 3rd expression
    switch(Zcase) {
    case 1:
      oi = isingle_ox_x1_x2(z[i], oz, z1, z2);
      break;
    case 2:
      oi = dsingle_ox_x1_x2(zd[i], oz, zd1, zd2);
      break;
    case 3:
      oi = C[i];
      break;
    case 4:
      oi = !C[i];
      break;
    }
    if (ampersand) {
      if (!oi) {
        continue;
      }
      out += 1;
    } else {
      if (oi) {
        out += 1;
        continue;
      }
    }
  }
  return ScalarLength(out);
}


