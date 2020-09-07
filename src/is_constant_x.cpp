#include "cpphutils.h"

namespace impl {
template <int RTYPE>
bool is_constant_(const Vector<RTYPE>& x, int nThread)
{
  R_xlen_t N = x.length();
  if (N <= 1) {
    return true;
  }

  if (nThread <= 1) {
    for (R_xlen_t i = 1; i < N; ++i) {
      if (x[i] != x[0]) {
        return false;
      }
    }
    return true;
  }

  bool o = x[0] == x[N - 1];
#pragma omp parallel for num_threads(nThread) reduction(&& : o)
  for (R_xlen_t i = 1; i < N; ++i) {
    if (o) {
      o = x[i] == x[0];
    }
  }
  return o;
}

}

// [[Rcpp::export(rng = false)]]
bool all_na_real(DoubleVector x, int nThread = 1) {
  R_xlen_t N = x.length();
  bool o = R_IsNA(x[0]);
  if (o) {
#pragma omp parallel for num_threads(nThread) reduction(&& : o)
    for (R_xlen_t i = 1; i < N; ++i) {
      o = o && R_IsNA(x[0]);
    }
  }
  return o;
}


// [[Rcpp::export(rng = false)]]
bool do_is_constant(SEXP x, int nThread = 1) {
  switch(TYPEOF(x)) {
  case NILSXP:
    return true;
  case LGLSXP:
    return impl::is_constant_(as<LogicalVector>(x), nThread);
  case INTSXP:
    return impl::is_constant_(as<IntegerVector>(x), nThread);
  case REALSXP:
    return impl::is_constant_(as<DoubleVector>(x), nThread);
  case STRSXP:
    return impl::is_constant_(as<CharacterVector>(x), nThread);
  case RAWSXP:
    return impl::is_constant_(as<RawVector>(x), nThread);
  }
  return false;
}
