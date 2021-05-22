#include "hutilscpp.h"



// # nocov start

bool has_openmp() {
#ifdef _OPENMP
  return true;
#endif
  return false;
}

#define OPENMP_REQUEST_OK 0
#define OPENMP_THREADS_NEGATIVE 1
#define OPENMP_THREADS_EXCEEDED 2


SEXP Chas_openmp() {
  return ScalarLogical(has_openmp());
}

// returns a list of three elements (intended to be passed to an if statement
// immediately after so no NA_LOGICALs)
// whether to warn/error
// whether to warn
// messages

SEXP Cdiagnose_omp(SEXP Threads_requested) {
  int threads_requested = asInteger(Threads_requested);


#ifndef _OPENMP
  return ScalarInteger(0);
#endif

  int n_procs = 1;
#ifdef _OPENMP
  n_procs = omp_get_num_procs();
#endif

  if (threads_requested > 0 && threads_requested <= n_procs) {
    return ScalarInteger(OPENMP_REQUEST_OK);
  }

  if (threads_requested < 0) {
    return ScalarInteger(OPENMP_THREADS_NEGATIVE);
  }
  if (threads_requested > n_procs) {
    return ScalarInteger(OPENMP_THREADS_EXCEEDED);
  }

  return ScalarInteger(-1);
}

// # nocov end

