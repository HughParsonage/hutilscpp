// #include "hutilscpp.h"
//
// unsigned int pcg_hash(unsigned int input)
// {
//   unsigned int state = input * 747796405u + 2891336453u;
//   unsigned int word = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
//   return (word >> 22u) ^ word;
// }
//
//
// SEXP Cpcg_hash(SEXP n, SEXP r, SEXP nthreads) {
//   int nThread = as_nThread(nthreads);
//   if (nThread > 32) {
//     nThread = 32;
//   }
//   unsigned int N = asReal(n);
//
//   unsigned int States[32] = {0};
//   if (TYPEOF(r) == INTSXP && xlength(r) >= 32) {
//     for (int i = 0; i < 32; ++i) {
//       States[i] = (unsigned int)INTEGER_ELT(r, i);
//     }
//   } else {
//     for (unsigned int i = 0; i < 32; ++i) {
//       States[i] = i + 2U;
//     }
//   }
//   SEXP ans = PROTECT(allocVector(INTSXP, N));
//   int * restrict ansp = INTEGER(ans);
// #if defined _OPENMP && _OPENMP >= 201511
// #pragma omp parallel for num_threads(nThread) schedule(static)
// #endif
//   for (unsigned int i = 0; i < N; ++i) {
// #ifdef _OPENMP
//     int oi = omp_get_thread_num();
// #else
//     int oi = (i & 31U);
// #endif
//     unsigned int new_si = pcg_hash(States[oi]);
//     ansp[i] = new_si;
//     States[oi] = new_si;
//   }
//   UNPROTECT(1);
//   return ans;
// }
//
// SEXP firstAbsentInt(SEXP xx, SEXP From, SEXP nthreads) {
//   if (TYPEOF(xx) != INTSXP) {
//     error("Not an int.");
//   }
//   R_xlen_t N = xlength(xx);
//   if (N >= 4294967295U) {
//     return R_NilValue;
//   }
//   int nThread = as_nThread(nthreads);
//   if (nThread > 32) {
//     nThread = 32;
//   }
//   const int * xp = INTEGER(xx);
//   unsigned char * all_ints = calloc(4294967295U, sizeof(char));
//   if (all_ints == NULL) {
//     free(all_ints);
//     warning("Unable to allocate.");
//     return R_NilValue;
//   }
//
//
// #if defined _OPENMP && _OPENMP >= 201511
// #pragma omp parallel for num_threads(nThread)
// #endif
//   for (R_xlen_t i = 0; i < N; ++i) {
//     unsigned int ui = xp[i];
//     all_ints[ui] |= 1;
//   }
//
//   unsigned int i = 0;
//   for (; i < 4294967294U; ++i) {
//     unsigned char aii = all_ints[i];
//     if (!aii) {
//       free(all_ints);
//       return ScalarInteger((int)i);
//     }
//   }
//   free(all_ints);
//   return ScalarInteger(0);
// }
//
// SEXP firstAbsentIntBuf(SEXP xx, SEXP From, SEXP nthreads) {
//   if (TYPEOF(xx) != INTSXP) {
//     error("Not an int.");
//   }
//   R_xlen_t N = xlength(xx);
//   if (N >= 4294967295U) {
//     return R_NilValue;
//   }
//   int nThread = as_nThread(nthreads);
//   if (nThread > 32) {
//     nThread = 32;
//   }
//   const int * xp = INTEGER(xx);
//   unsigned char * all_ints = calloc(16777216U, sizeof(char));
//   if (all_ints == NULL) {
//     free(all_ints);
//     warning("Unable to allocate.");
//     return R_NilValue;
//   }
//   for (int b = 0; b < 256; ++b) {
//     if (b) {
//       memset(all_ints, 0, 16777216U * sizeof(char));
//     }
//     const unsigned int b_shift = 16777216U * b;
//     const unsigned int b_right = b_shift + 16777216U;
//
// #if defined _OPENMP && _OPENMP >= 201511
// #pragma omp parallel for num_threads(nThread)
// #endif
//     for (unsigned int i = 0; i < N; ++i) {
//       unsigned int xpi = xp[i];
//       if (xpi > b_right || xpi < b_shift) {
//         continue;
//       }
//       unsigned int ui = xpi & 16777215U;
//       all_ints[ui] |= 1;
//     }
//
//
//     for (unsigned int i = 0; i < 16777216U; ++i) {
//       unsigned char aii = all_ints[i];
//       if (!aii) {
//         free(all_ints);
//         return ScalarInteger((int)(i + b_shift));
//       }
//     }
//   }
//   free(all_ints);
//   return ScalarInteger(0);
// }
//
// SEXP COneTo1024(SEXP x, SEXP nthreads) {
//   if (TYPEOF(x) != INTSXP) {
//     return R_NilValue;
//   }
//   int nThread = as_nThread(nthreads);
//   unsigned char o[1024] = {0};
//
//   const int * xp = INTEGER(x);
//   R_xlen_t N = xlength(x);
// #if defined _OPENMP && _OPENMP >= 201511
// #pragma omp parallel for num_threads(nThread)
// #endif
//   for (R_xlen_t i = 0; i < N; ++i) {
//     unsigned xpi = xp[i];
//     if (xpi < 1024) {
//       o[xpi] |= 1;
//     }
//   }
//   SEXP ans = PROTECT(allocVector(INTSXP, 1024));
//   int * ansp = INTEGER(ans);
//   for (int i = 0; i < 1024; ++i) {
//     ansp[i] = o[i];
//   }
//   UNPROTECT(1);
//   return ans;
// }
//
// SEXP CTabulate256(SEXP x, SEXP nthreads) {
//   if (TYPEOF(x) != INTSXP || xlength(x) >= 4294967295U) {
//     return R_NilValue;
//   }
//   int nThread = as_nThread(nthreads);
//   R_xlen_t N = xlength(x);
//   const int * xp = INTEGER(x);
//
//   unsigned int o[256] = {0};
// #if defined _OPENMP && _OPENMP >= 201511
// #pragma omp parallel for num_threads(nThread) reduction(+ : o[:256])
// #endif
//   for (unsigned int i = 0; i < N; ++i) {
//     unsigned int xi = xp[i];
//     unsigned int oi = xi & 255U;
//     o[oi] += 1;
//   }
//   SEXP ans = PROTECT(allocVector(INTSXP, 256));
//   int * ansp = INTEGER(ans);
//   for (int i = 0; i < 256; ++i) {
//     ansp[i] = o[i];
//   }
//   UNPROTECT(1);
//   return ans;
// }
//
// SEXP CNot(SEXP x, SEXP nthreads) {
//   int nThread = as_nThread(nthreads);
//   R_xlen_t N = xlength(x);
//   int * xp = LOGICAL(x);
// #if defined _OPENMP && _OPENMP >= 201511
// #pragma omp parallel for num_threads(nThread)
// #endif
//   for (R_xlen_t i = 0; i < N; ++i) {
//     int xpi = xp[i];
//     xp[i] = !xpi;
//   }
//   return x;
// }
//
