#include "hutilscpp.h"

int minii(int a, int b) {
  return (a < b) ? a : b;
}
int mini3(int a, int b, int c) {
  return minii(minii(a, b),
               minii(a, c));
}
int maxii(int a, int b) {
  return (a < b) ? b : a;
}
int maxi3(int a, int b, int c) {
  return maxii(maxii(a, b),
               maxii(a, c));
}

double minid(int a, double b) {
  return (a < b) ? a : b;
}
double maxid(int a, double b) {
  return (a < b) ? b : a;
}

double mindd(double a, double b) {
  return (a < b) ? a : b;
}
double maxdd(double a, double b) {
  return (a < b) ? b : a;
}

double mind3(double a, double b, double c) {
  return mindd(mindd(a, b),
               mindd(a, c));
}
double maxd3(double a, double b, double c) {
  return maxdd(maxdd(a, b),
               maxdd(a, c));
}

double Mind(const double * x, R_xlen_t N, int nThread) {
  if (N == 0) {
    return R_PosInf; // # nocov
  }
  double o = x[0];
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(min : o)
#endif
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i] < o) {
      o = x[i];
    }
  }
  return o;
}

double Maxd(const double * x, R_xlen_t N, int nThread) {
  if (N == 0) {
    return R_NegInf; // # nocov
  }
  double o = x[0];
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(max : o)
#endif
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i] > o) {
      o = x[i];
    }
  }
  return o;
}

int Maxi(const int * x, R_xlen_t N, int nThread) {
  if (N == 0) {
    return NA_INTEGER; // # nocov
  }
  int o = x[0];
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(max : o)
#endif
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i] > o) {
      o = x[i];
    }
  }
  return o;
}




