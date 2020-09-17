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
  if (op == OP_BO) {
    // even (F, T) can never occur
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
      if (y[0] == TRUE && y[1] == FALSE) {
        return 0;
      }
      // c(NA, TRUE) => x <= TRUE
      y[0] = (y[0] == NA_LOGICAL) ? FALSE : y[0];
      y[1] = (y[1] == NA_LOGICAL) ? TRUE  : y[1];

      const bool onlyTRUE  = y[0] == TRUE;
      const bool onlyFALSE = y[1] == FALSE;

      // otherwise we just use the normal:
      for (R_xlen_t k = 0; k < N; ++k) {
        R_xlen_t i = reverse ? (N - k - 1) : k;
        if (onlyTRUE) {
          if (x[i] == TRUE) {
            return i + 1;
          }
          continue;
        }
        if (onlyFALSE) {
          if (x[i] == FALSE) {
            return i + 1;
          }
          continue;
        }
        if (x[i] != NA_LOGICAL) {
          return i + 1;
        }
      }
      return 0;
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

// Used for %in% to determine whether RHS should trigger TRUE if lhs is NA
bool table_with_na(SEXP Y, int op) {
  if (op != OP_IN) {
    return false;
  }
  if (TYPEOF(Y) == INTSXP) {
    IntegerVector y = Y;
    R_xlen_t N = y.length();
    for (R_xlen_t i = 0; i < N; ++i) {
      if (y[i] == NA_INTEGER) {
        return true;
      }
    }
  } else {
    DoubleVector y = Y;
    R_xlen_t N = y.length();
    for (R_xlen_t i = 0; i < N; ++i) {
      if (ISNAN(y[i])) {
        return true;
      }
    }
  }
  return false;
}

// [[Rcpp::export(rng = false)]]
R_xlen_t do_which_first_n(SEXP X, SEXP Y, int op, bool last = false) {
  const bool table_has_na = table_with_na(Y, op);
   if (TYPEOF(X) == INTSXP && TYPEOF(Y) == INTSXP) {
     IntegerVector x = X;
     IntegerVector y = Y;
     R_xlen_t N = x.length(), Ny = y.length();
     if (N == 0 || Ny == 0) {
       return 0; // # nocov
     }

     if (op == OP_BW ||
         op == OP_BO ||
         op == OP_BC) {
       if (Ny != 2) {
         stop("%between% expected rhs of length-2");
       }
       if (y[0] == NA_INTEGER && y[1] == NA_INTEGER) {
         return last ? N : 1;
       }
       if (y[0] == NA_INTEGER) {
         if (op == OP_BW) {
           op = OP_LE;
         } else if (op == OP_BO) {
           op = OP_LT;
         } else {
           op = OP_GE;
         }
         y[0] = y[1];
       } else if (y[1] == NA_INTEGER) {
         if (op == OP_BW) {
           op = OP_GE;
         } else if (op == OP_BO) {
           op = OP_GT;
         } else {
           op = OP_LE;
         }
         // y[0] already correct
       }
     }
     const int oq = op;

     const bool y1_is_i = (Ny == N && oq < OP_IN);
     const bool y2_is_1 = (Ny >= 2 && oq >= OP_IN);

     for (R_xlen_t k = 0; k < N; ++k) {
       R_xlen_t i = last ? (N - k - 1) : k;
       int xi = x[i];
       // For operators != == >= <= > < we treat it as
       //    x > y  if y has same length as x
       //    x > 1  if y has length one [which we assume occurs whenever Ny != N]
       // for other operators %in% %between% etc the operation is not
       // 'parallel' so the first element of y is always y1
       int y1 = y1_is_i ? y[i] : y[0];
       int y2 = y2_is_1 ? y[1] : y[0];
       if (oq != OP_IN) {
         if (single_ox_x1_x2(xi, oq, y1, y2)) {
           return i + 1;
         }
       } else {
         if (table_has_na && xi == NA_INTEGER) {
           return i + 1;
         }
         for (R_xlen_t j = 0; j < Ny; ++j) {
           int yj = y[j];
           if (xi == yj) {
             return i + 1;
           }
         }
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

     if (op == OP_BW ||
         op == OP_BO ||
         op == OP_BC) {
       if (Ny != 2) {
         stop("%between% expected rhs of length-2");
       }
       if (ISNAN(y[0]) && ISNAN(y[1])) {
         return last ? N : 1;
       }
       if (ISNAN(y[0])) {
         if (op == OP_BW) {
           op = OP_LE;
         } else if (op == OP_BO) {
           op = OP_LT;
         } else {
           op = OP_GE;
         }
         y[0] = y[1];
       } else if (ISNAN(y[1])) {
         if (op == OP_BW) {
           op = OP_GE;
         } else if (op == OP_BO) {
           op = OP_GT;
         } else {
           op = OP_LE;
         }
         // y[0] already correct
       }
     }
     const int oq = op;

     const bool y1_is_i = (Ny == N && op < OP_IN);
     const bool y2_is_1 = (Ny >= 2 && op >= OP_IN);

     for (R_xlen_t k = 0; k < N; ++k) {
       R_xlen_t i = last ? (N - k - 1) : k;
       double xi = x[i];
       double y1 = y1_is_i ? y[i] : y[0];
       double y2 = y2_is_1 ? y[1] : y[0];
       if (oq != OP_IN) {
         if (single_ox_x1_x2(xi, oq, y1, y2)) {
           return i + 1;
         }
       } else {
         if (table_has_na && ISNAN(xi)) {
           return i + 1;
         }
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

     if (op == OP_BW ||
         op == OP_BO ||
         op == OP_BC) {
       if (Ny != 2) {
         stop("%between% expected rhs of length-2");
       }
       if (y[0] == NA_INTEGER && y[1] == NA_INTEGER) {
         return last ? N : 1;
       }
       if (y[0] == NA_INTEGER) {
         if (op == OP_BW) {
           op = OP_LE;
         } else if (op == OP_BO) {
           op = OP_LT;
         } else {
           op = OP_GE;
         }
         y[0] = y[1];
       } else if (y[1] == NA_INTEGER) {
         if (op == OP_BW) {
           op = OP_GE;
         } else if (op == OP_BO) {
           op = OP_GT;
         } else {
           op = OP_LE;
         }
         // y[0] already correct
       }
     }

     const int oq = op;

     const bool y1_is_i = (Ny == N && op < OP_IN);
     const bool y2_is_1 = (Ny >= 2 && op >= OP_IN);

     for (R_xlen_t k = 0; k < N; ++k) {
       R_xlen_t i = last ? (N - k - 1) : k;
       double xi = x[i];
       double y1 = y1_is_i ? y[i] : y[0];
       double y2 = y2_is_1 ? y[1] : y[0];
       if (oq != OP_IN) {
         if (single_ox_x1_x2(xi, oq, y1, y2)) {
           return i + 1;
         }
       } else {
         if (table_has_na && ISNAN(xi)) {
           return i + 1;
         }
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

     if (op == OP_BW ||
         op == OP_BO ||
         op == OP_BC) {
       if (Ny != 2) {
         stop("%between% expected rhs of length-2");
       }
       if (ISNAN(y[0]) && ISNAN(y[1])) {
         return last ? N : 1;
       }
       if (ISNAN(y[0])) {
         if (op == OP_BW) {
           op = OP_LE;
         } else if (op == OP_BO) {
           op = OP_LT;
         } else {
           op = OP_GE;
         }
         y[0] = y[1];
       } else if (ISNAN(y[1])) {
         if (op == OP_BW) {
           op = OP_GE;
         } else if (op == OP_BO) {
           op = OP_GT;
         } else {
           op = OP_LE;
         }
         // y[0] already correct
       }
     }
     const int oq = op;

     const bool y1_is_i = (Ny == N && op < OP_IN);
     const bool y2_is_1 = (Ny >= 2 && op >= OP_IN);

     for (R_xlen_t k = 0; k < N; ++k) {
       R_xlen_t i = last ? (N - k - 1) : k;
       if (oq != OP_IN) {
         double xi = x[i];
         double y1 = y1_is_i ? y[i] : y[0];
         double y2 = y2_is_1 ? y[1] : y[0];
         if (single_ox_x1_x2(xi, oq, y1, y2)) {
           return i + 1;
         }
       } else {
         if (table_has_na && x[i] == NA_INTEGER) {
           return i + 1;
         }
         double xi = x[i];
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





