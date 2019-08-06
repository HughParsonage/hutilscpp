//' @name do_pmaxC
//' @title Internal pmaxC helpers
//' @description Internal functions used when the overheads of assertions
//' would be too expensive. The \code{_IP_} flavours modify in place.
//' @param x A numeric/integer vector.
//' @param a A single numeric/integer.
//' @param in_place Modify \code{x} in place?
//' @export do_pmaxC_dbl do_pmaxC_int do_pmax0 do_pmaxIPint0 do_pmaxIPnum0

#include <Rcpp.h>
#include "cpphutils.h"
using namespace Rcpp;
// #include <RcppParallel.h>
// using namespace Rcpp;
//
// struct Pmax : public RcppParallel::Worker {
//   struct Apply {
//     double mx;
//     Apply(double mx_)
//       : mx(mx_)
//     {}
//
//     double operator()(const double x) const
//     {
//       return x > mx ? x : mx;
//     }
//   };
//
//   const RcppParallel::RVector<double> input;
//   RcppParallel::RVector<double> output;
//
//   Apply f;
//
//   Pmax(const Rcpp::NumericVector input_,
//        Rcpp::NumericVector output_,
//        double mx_)
//     : input(input_), output(output_), f(mx_)
//   {}
//
//   void operator()(std::size_t begin, std::size_t end)
//   {
//     std::transform(
//       input.begin() + begin,
//       input.begin() + end,
//       output.begin() + begin,
//       f
//     );
//   }
// };
//
// // [[Rcpp::export]]
// Rcpp::NumericVector pmaxC(Rcpp::NumericVector x, double a)
// {
//   Rcpp::NumericVector res = Rcpp::no_init_vector(x.size());
//   Pmax p(x, res, a);
//   RcppParallel::parallelFor(0, x.size(), p);
//
//   return res;
// }

//' @rdname do_pmaxC
// [[Rcpp::export]]
NumericVector do_pmaxC_dbl(NumericVector x, double a, bool in_place = false) {
  R_xlen_t n = x.length();
  NumericVector out = in_place ? NumericVector(x) : NumericVector(clone(x));

  for (R_xlen_t i = 0; i < n; ++i) {
    if (x[i] < a) {
      out[i] = a;
    }
  }
  return out;
}

//' @rdname do_pmaxC
// [[Rcpp::export]]
IntegerVector do_pmaxC_int(IntegerVector x, int a, bool in_place = false) {
  R_xlen_t n = x.length();
  IntegerVector out = in_place ? IntegerVector(x) : IntegerVector(clone(x));

  for (R_xlen_t i = 0; i < n; ++i) {
    if (x[i] < a) {
      out[i] = a;
    }
  }

  return out;
}

//' @rdname do_pmaxC
// [[Rcpp::export]]
NumericVector do_pmax0(NumericVector x, bool in_place = false) {
  R_xlen_t n = x.length();
  NumericVector out = in_place ? NumericVector(x) : NumericVector(clone(x));

  for (R_xlen_t i = 0; i < n; ++i) {
    if (x[i] < 0) {
      out[i] = 0;
    }
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector do_pmaxIP_int(IntegerVector x, int a) {
  R_xlen_t n = x.length();
  for (R_xlen_t i = 0; i < n; ++i) {
    if (x[i] < a) {
      x[i] = a;
    }
  }
  return x;
}

// [[Rcpp::export]]
DoubleVector do_pmaxIP_dbl(DoubleVector x, double a) {
  R_xlen_t n = x.length();
  for (R_xlen_t i = 0; i < n; ++i) {
    if (x[i] < a) {
      x[i] = a;
    }
  }
  return x;
}

//' @rdname do_pmaxC
// [[Rcpp::export]]
NumericVector do_pmaxIPnum0(NumericVector x) {
  R_xlen_t n = x.length();

  for (R_xlen_t i = 0; i < n; ++i) {
    if (x[i] < 0) {
      x[i] = 0;
    }
  }
  return x;
}

//' @rdname do_pmaxC
// [[Rcpp::export]]
IntegerVector do_pmaxIPint0(IntegerVector x) {
  R_xlen_t n = x.length();

  for (R_xlen_t i = 0; i < n; ++i) {
    if (x[i] < 0) {
      x[i] = 0;
    }
  }
  return x;
}


