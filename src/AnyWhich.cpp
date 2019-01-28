#include <Rcpp.h>
using namespace Rcpp;

//' @title Quickly verify (and locate) the existence of a breach.
//' @name AnyWhich
//' @description Used when a single instance is likely to occur and be important to detect quickly
//' (in a sufficiently large integer vector).
//' @param x An integer vector.
//' @param a A (single) integer. That which is to be compared.
//' @param gt,lt,eq Booleans, whether or not the comparison is greater than, less than, or equal to.
//' Only \code{gt} and \code{lt} are mutually exclusive. If all \code{FALSE}, find the first instance
//' where none are equal (i.e. does \code{x} have more than one distinct value).
//'

// [[Rcpp::export]]
int AnyWhich_dbl(NumericVector x, double a, bool gt, bool lt, bool eq) {
  int N = x.size();

  if (gt) {
    if (lt) {
      stop("gt and lt were both TRUE.");
    }
    if (eq) {
      for (int i = 0; i < N; ++i) {
        if (x[i] >= a) {
          return ++i; // 0 vs 1 indexing
        }
      }
    } else {
      for (int i = 0; i < N; ++i) {
        if (x[i] > a) {
          return ++i;
        }
      }
    }
  } else if (lt) {
    if (eq) {
      for (int i = 0; i < N; ++i) {
        if (x[i] <= a) {
          return ++i;
        }
      }
    } else {
      for (int i = 0; i < N; ++i) {
        if (x[i] < a) {
          return ++i;
        }
      }
    }
  } else {
    if (eq) {
      // Just equality
      for (int i = 0; i < N; ++i) {
        if (x[i] == a) {
          return ++i;
        }
      }
    } else {
      // Just inequality
      for (int i = 0; i < N; ++i) {
        if (x[i] != a) {
          return ++i;
        }
      }
    }
  }

  return 0;
}

// [[Rcpp::export]]
int AnyWhich_int(IntegerVector x, int a, bool gt, bool lt, bool eq) {
  int N = x.size();

  if (gt) {
    if (lt) {
      stop("gt and lt were both TRUE.");
    }
    if (eq) {
      for (int i = 0; i < N; ++i) {
        if (x[i] >= a) {
          return ++i; // 0 vs 1 indexing
        }
      }
    } else {
      for (int i = 0; i < N; ++i) {
        if (x[i] > a) {
          return ++i;
        }
      }
    }
  } else if (lt) {
    if (eq) {
      for (int i = 0; i < N; ++i) {
        if (x[i] <= a) {
          return ++i;
        }
      }
    } else {
      for (int i = 0; i < N; ++i) {
        if (x[i] < a) {
          return ++i;
        }
      }
    }
  } else {
    if (eq) {
      // Just equality
      for (int i = 0; i < N; ++i) {
        if (x[i] == a) {
          return ++i;
        }
      }
    } else {
      // Just inequality
      for (int i = 0; i < N; ++i) {
        if (x[i] != a) {
          return ++i;
        }
      }
    }
  }

  return 0;
}

// [[Rcpp::export]]
int AnyWhichInDbl (DoubleVector x, DoubleVector Table) {
  int N = x.size();
  int M = Table.size();

  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < M; ++j) {
      if (x[i] == Table[j]) {
        return ++i;
      }
    }
  }
  return 0;
}

// [[Rcpp::export]]
int AnyWhichInInt (IntegerVector x, IntegerVector Table) {
  int N = x.size();
  int M = Table.size();

  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < M; ++j) {
      if (x[i] == Table[j]) {
        return ++i;
      }
    }
  }
  return 0;
}


