#include "cpphutils.h"

// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_first(LogicalVector x) {
  R_xlen_t N = x.size();
  for (R_xlen_t i = 0; i < N; ++i) {
    if (x[i] == TRUE) {
      return ++i;
    }
  }
  return 0;
}

// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_last(LogicalVector x) {
  R_xlen_t N = x.size();
  for (R_xlen_t i = N - 1; i >= 0; --i) {
    if (x[i] == TRUE) {
      return ++i;
    }
  }
  return 0;
}

// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_first_false (LogicalVector x) {
  R_xlen_t N = x.size();
  for (R_xlen_t i = 0; i < N; ++i) {
    if (x[i] == FALSE) {
      return ++i;
    }
  }
  return 0;
}

// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_last_false (LogicalVector x) {
  R_xlen_t N = x.size();
  for (R_xlen_t i = N - 1; i >= 0; --i) {
    if (x[i] == FALSE) {
      return ++i;
    }
  }
  return 0;
}

// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_first_notTRUE(LogicalVector x) {
  R_xlen_t N = x.size();
  for (R_xlen_t i = 0; i < N; ++i) {
    if (x[i] != TRUE) {
      return ++i;
    }
  }
  return 0;
}

// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_last_notTRUE(LogicalVector x) {
  R_xlen_t N = x.size();
  for (R_xlen_t i = N - 1; i >= 0; --i) {
    if (x[i] != TRUE) {
      return ++i;
    }
  }
  return 0;
}

// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_last_notFALSE(LogicalVector x) {
  R_xlen_t N = x.size();
  for (R_xlen_t i = N - 1; i >= 0; --i) {
    if (x[i] != FALSE) {
      return ++i;
    }
  }
  return 0;
}

namespace hcwf {
template <int RTYPE>
R_xlen_t do_which_firstNA_(const Vector<RTYPE>& x) {
  R_xlen_t N = x.length();
  for (R_xlen_t i = 0; i < N; ++i) {
    if (Vector<RTYPE>::is_na(x[i])) {
      return i + 1;
    }
  }
  return 0;
}

template <int RTYPE>
R_xlen_t do_which_lastNA_(const Vector<RTYPE>& x) {
  R_xlen_t N = x.length();
  for (R_xlen_t i = N - 1; i >= 0; --i) {
    if (Vector<RTYPE>::is_na(x[i])) {
      return i + 1;
    }
  }
  return 0;
}

}

// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_firstNA(SEXP x) {
  switch(TYPEOF(x)) {
  case NILSXP:
    return 0;
  case LGLSXP:
    return hcwf::do_which_firstNA_(as<LogicalVector>(x));
  case INTSXP:
    return hcwf::do_which_firstNA_(as<IntegerVector>(x));
  case REALSXP:
    return hcwf::do_which_firstNA_(as<DoubleVector>(x));
  case STRSXP:
    return hcwf::do_which_firstNA_(as<CharacterVector>(x));
  case RAWSXP:
    return 0;
  }
  return 0; // # nocov
}

// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_lastNA(SEXP x) {
  switch(TYPEOF(x)) {
  case NILSXP:
    return 0;
  case LGLSXP:
    return hcwf::do_which_lastNA_(as<LogicalVector>(x));
  case INTSXP:
    return hcwf::do_which_lastNA_(as<IntegerVector>(x));
  case REALSXP:
    return hcwf::do_which_lastNA_(as<DoubleVector>(x));
  case STRSXP:
    return hcwf::do_which_lastNA_(as<CharacterVector>(x));
  case RAWSXP:
    return 0;
  }
  return 0; // # nocov
}






// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_first_lgl_lgl_op(LogicalVector x, LogicalVector y, int op, bool reverse = false) {
  R_xlen_t N = x.length(), Ny = y.length();
  if (N == 0 || Ny == 0) {
    return 0;
  }
  const bool len_eq = Ny == N;
  const bool len1 = Ny == 1;
  if (!len_eq && !len1 && op != OP_IN && op != OP_BW) {
    stop("Lengths differ."); // # nocov
  }
  if (op == OP_IN || op == OP_BW) {
    if (op == OP_BW && Ny != 2) {
      stop("%between% expects RHS to be a vector of length-2.");
    }
    bool hasNA = false;
    bool hasTRUE = false;
    bool hasFALSE = false;

    for (R_xlen_t j = 0; j < Ny; ++j) {
      hasNA = hasNA || y[j] == NA_LOGICAL;
      hasTRUE = hasTRUE || y[j] == TRUE;
      hasFALSE = hasFALSE || y[j] == FALSE;
    }

    if (hasNA && hasTRUE && hasFALSE) {
      if (reverse) {
        return N;
      } else {
        return 1;
      }
    }

    // Two values, for %between%, we need F,F F,T or T,T
    // otherwise will never occur so return 0 now
    if (op == OP_BW) {
      if (y[0] == TRUE && y[1] != TRUE) {
        return 0;
      }
      if (y[1] == FALSE && y[0] != FALSE) {
        return 0;
      }

      // otherwise we just use the normal:
    }
    //
    for (R_xlen_t k = 0; k < N; ++k) {
      R_xlen_t i = reverse ? (N - k - 1) : k;
      if (hasNA && x[i] == NA_LOGICAL) {
        return i + 1;
      }
      if (hasTRUE && x[i] == TRUE) {
        return i + 1;
      }
      if (hasFALSE && x[i] == FALSE) {
        return i + 1;
      }
    }
    return 0;
  }

  for (R_xlen_t k = 0; k < N; ++k) {
    R_xlen_t i = reverse ? (N - k - 1) : k;
    int xi = x[i];
    int y1 = len1 ? y[0] : y[i];
    int y2 = (N == 2) ? y[1] : y[0];
    if (single_ox_x1_x2(xi, op, y1, y2)) {
      return i + 1;
    }
  }
  return 0;
}

// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_first_n(SEXP X, SEXP Y, int op, bool last = false) {
   if (TYPEOF(X) == INTSXP && TYPEOF(Y) == INTSXP) {
     IntegerVector x = X;
     IntegerVector y = Y;
     R_xlen_t N = x.length(), Ny = y.length();
     if (N == 0 || Ny == 0) {
       return 0; // # nocov
     }

     for (R_xlen_t k = 0; k < N; ++k) {
       R_xlen_t i = last ? (N - k - 1) : k;
       int yi = (Ny == N) ? y[i] : y[0];
       int xi = x[i];
       switch (op) {
       case 1:
         if (xi != yi) return i + 1;
         break;
       case 2:
         if (xi == yi) return i + 1;
         break;
       case 3:
         if (xi >= yi) return i + 1;
         break;
       case 4:
         if (xi <= yi) return i + 1;
         break;
       case 5:
         if (xi >  yi) return i + 1;
         break;
       case 6:
         if (xi <  yi) return i + 1;
         break;
       case 7:
         for (R_xlen_t j = 0; j < Ny; ++j) {
           int yj = y[j];
           if (xi == yj) {
             return i + 1;
           }
         }
         break;
       case 8:
         if (xi >= y[0] && xi <= y[1]) {
           return i + 1;
         }
         break;
       case 9:
         if (xi > y[0] && xi < y[1]) {
           return i + 1;
         }
         break;
       case 10:
         if (xi <= y[0] || xi >= y[1]) {
           return i + 1;
         }
         break; // # nocov
       }
     }
     return 0;
   }



   if (TYPEOF(X) == REALSXP && TYPEOF(Y) == REALSXP) {
     DoubleVector x = X;
     DoubleVector y = Y;
     R_xlen_t N = x.length(), Ny = y.length();
     if (N == 0 || Ny == 0) {
       return 0; // # nocov
     }

     for (R_xlen_t k = 0; k < N; ++k) {
       R_xlen_t i = last ? (N - k - 1) : k;
       double xi = x[i];
       double y1 = (Ny == N && op < OP_IN) ? y[i] : y[0];
       double y2 = (op >= OP_IN) ? y[1] : y[0];
       if (op != OP_IN) {
         if (single_ox_x1_x2(xi, op, y1, y2)) {
           return i + 1;
         }
       } else {
         for (R_xlen_t j = 0; j < Ny; ++j) {
           double yj = y[j];
           if (xi == yj) {
             return i + 1;
           }
         }
       }
     }
     return 0;
   }


   if (TYPEOF(X) == REALSXP && TYPEOF(Y) == INTSXP) {

     DoubleVector x = X;
     IntegerVector y = Y;
     R_xlen_t N = x.length(), Ny = y.length();
     if (N == 0 || Ny == 0) {
       return 0; // # nocov
     }

     for (R_xlen_t k = 0; k < N; ++k) {
       R_xlen_t i = last ? (N - k - 1) : k;
       double xi = x[i];
       double y1 = (Ny == N && op < OP_IN) ? y[i] : y[0];
       double y2 = (op >= OP_IN) ? y[1] : y[0];
       if (op != OP_IN) {
         if (single_ox_x1_x2(xi, op, y1, y2)) {
           return i + 1;
         }
       } else {
         for (R_xlen_t j = 0; j < Ny; ++j) {
           double yj = y[j];
           if (xi == yj) {
             return i + 1;
           }
         }
       }
     }
     return 0;
   }

   if (TYPEOF(X) == INTSXP && TYPEOF(Y) == REALSXP) {
     IntegerVector x = X;
     DoubleVector y = Y;
     R_xlen_t N = x.length(), Ny = y.length();
     if (N == 0 || Ny == 0) {
       return 0; // # nocov
     }

     for (R_xlen_t k = 0; k < N; ++k) {
       R_xlen_t i = last ? (N - k - 1) : k;
       double xi = x[i];
       double y1 = (Ny == N && op < OP_IN) ? y[i] : y[0];
       double y2 = (op >= OP_IN) ? y[1] : y[0];
       if (op != OP_IN) {
         if (single_ox_x1_x2(xi, op, y1, y2)) {
           return i + 1;
         }
       } else {
         for (R_xlen_t j = 0; j < Ny; ++j) {
           double yj = y[j];
           if (xi == yj) {
             return i + 1;
           }
         }
       }
     }
     return 0;
   }

   warning("Internal error: do_which_first() 2020-09-14:530"); // # nocov
   return 0; // # nocov
}





