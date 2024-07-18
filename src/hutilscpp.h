#ifndef hutilsc_H
#define hutilsc_H

#if _OPENMP
#include <omp.h>
#endif

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rversion.h>
#include <stdint.h> // for uint64_t rather than unsigned long long
#include <stdbool.h>
#include <math.h>
#include <ctype.h>



#if (__GNUC__ > 7) || \
((__GNUC__ == 7) && (__GNUC_MINOR__ > 3))
#define BUILTIN_ADD_OVERFLOW_EXIST
#endif

#define DEBUG 0

// Error codes
#define RHS_BAD_LEN -2
#define BW_LEN_NE2 -3
#define UNSUPPORTED_OP -4
#define UNSUPPORTED_NA -5
#define SUM1_UNSUPPORTED_TYPE -6
#define INCOMPAT_LEN -7
#define AND3_UNSUPPORTED_TYPEX -8
#define AND3_UNSUPPORTED_TYPEY -9
#define OR3__UNSUPPORTED_TYPEX -10
#define OR3__UNSUPPORTED_TYPEY -11


// int op = !(eq || gt || lt) ? 0 : (eq ? (gt ? 2 : (lt ? 3 : 1)) : (gt ? 4 : 5));
// != == >= <=  >  <
//  0  1  2  3  4  5
#define OP_NE 1
#define OP_EQ 2
#define OP_GE 3
#define OP_LE 4
#define OP_GT 5
#define OP_LT 6
#define OP_IN 7
#define OP_BW 8
#define OP_BO 9
#define OP_BC 10
#define OP_NI 11
#define OP_WB 12
#define OP_TRUE 13
#define OP_FALSE 14

#define NA_INT -2147483648

#define CF_LEN_1 1
#define CF_LEN_2 2
#define CF_LEN_N -1

#define DBL_INT 0
#define DBL_NAN 1
#define DBL_FRA 2
#define DBL_XHI 3
#define DBL_XLO 4

#define ORAND_EQ 0
#define ORAND_OR 1
#define ORAND_AND 2

// number of elements where we can just do a linear search for in
#define MAX_NAIVE_IN 30

// fastmatch_fastmatch
#define UCHAR_THRESH 100

#define FORLOOP1(content)                                      \
  for (R_xlen_t i = 0; i < N; ++i) {                           \
    content                                                     \
    }                                                          \

#if defined _OPENMP && _OPENMP >= 201511
#define FORLOOP(content)                                                \
_Pragma("omp parallel for num_threads(nThread)")                        \
  for (R_xlen_t i = 0; i < N; ++i) {                                    \
    content                                                             \
  }
#else
#define FORLOOP(content)                                       \
for (R_xlen_t i = 0; i < N; ++i) {                             \
  content                                                      \
}
#endif

#if defined _OPENMP && _OPENMP >= 201511
#define FORLOOP_ands(op, rhs)                                                 \
_Pragma("omp parallel for num_threads(nThread)")                              \
  for (R_xlen_t i = 0; i < N; ++i) {                                          \
    ansp[i] &= x[i] op rhs;                                                   \
  }
#else
#define FORLOOP_ands(op, rhs)                                   \
for (R_xlen_t i = 0; i < N; ++i) {                              \
  ansp[i] &= x[i] op rhs;                                       \
}
#endif

#if defined _OPENMP && _OPENMP >= 201511
#define FORLOOP_redsum(content)                                                        \
_Pragma("omp parallel for num_threads(nThread) reduction(+:o)")                        \
  for (R_xlen_t i = 0; i < N; ++i) {                                                   \
    content                                                                            \
  }
#else
#define FORLOOP_redsum(content)                                       \
for (R_xlen_t i = 0; i < N; ++i) {                                    \
  content                                                             \
}
#endif

#if defined _OPENMP && _OPENMP >= 201511
#define FORLOOP_redand(content)                                                        \
_Pragma("omp parallel for num_threads(nThread) reduction(&&:o)")                        \
  for (R_xlen_t i = 0; i < N; ++i) {                                                   \
    content                                                                            \
  }
#else
#define FORLOOP_redand(content)                                       \
for (R_xlen_t i = 0; i < N; ++i) {                                    \
  content                                                             \
}
#endif

#if defined _OPENMP && _OPENMP >= 201511
#define FORLOOP_xminmax(content)                                                         \
_Pragma("omp parallel for num_threads(nThread) reduction(min : xmin) reduction(max : xmax)")                        \
  for (R_xlen_t i = 0; i < N; ++i) {                                                    \
    content                                                                             \
  }
#else
#define FORLOOP_xminmax(content)                                       \
for (R_xlen_t i = 0; i < N; ++i) {                                    \
  content                                                             \
}
#endif





extern int tens[10];

// allocate
SEXP LogicalN(R_xlen_t N);
SEXP IntegerN(R_xlen_t N);
SEXP IntegerNNA(R_xlen_t N);
SEXP DoubleN(R_xlen_t N);
SEXP DoubleNNA(R_xlen_t N);
SEXP RawN(R_xlen_t N);

// altrep
bool is_altrep(SEXP x);

// between.c
bool betweeniiuu(unsigned int x, unsigned int a, unsigned b) ;
void uc_betweenidd(unsigned char * ansp, int ORAND, const int * xp, R_xlen_t N, int nThread, double y0, double y1);

int do_op2M(const char * x);
int sex2op(SEXP oo);
int rev_op(int op);
int inv_op(int op);

// asInteger2
int asInteger2(SEXP x);
bool is_true(SEXP x);

int cf_xlen(SEXP x, SEXP y);
int op_xlen2(int o);

// diagnose_omp
int as_nThread(SEXP x);
#if _OPENMP
#define AS_NTHREAD int nThread = as_nThread(nthreads);
#else
#define AS_NTHREAD do {;} while (0);
#endif

bool has_openmp(void);

// error-warnings.c
void assertInteger(SEXP x, const char * var);

SEXP ScalarLength(R_xlen_t o);

bool dsingle_ox_x1_x2(double x, int oix, double x1, double x2);
bool isingle_ox_x1_x2(int x, int oix, int x1, int x2);

// is_seq
bool is_seq(SEXP x);

bool do_is_safe2int(double x);
int dbl_is_int(double x);
int dbl2int(double x);
int why_dbl_isnt_int(double x);
int sex2int1(SEXP x);

R_xlen_t sum_isna(SEXP x, SEXP nthreads) ;

// character
bool string_equal(const char * x, const char * y);
bool string_equaln(const char * x, int nx, const char * y);

// Cpar_in.c
SEXP par_in_intchar(SEXP x, SEXP y, int nThread, int yminmax[2], bool opposite);

// extent.c
bool ithinner(const int * xp, R_xlen_t N, int nThread, unsigned int width, int * aminmax);

// fastmatch_fastmatch
SEXP fmatch(SEXP x, SEXP y, SEXP nonmatch, SEXP Fin, SEXP WhichFirst, SEXP nthreads);

// isntRaw
bool isntRaw(SEXP x);
bool isRaw(SEXP x);

float ssqrt_fast(float x);
unsigned int radix_find(int a, unsigned int x0, unsigned int x1, const int * k1, unsigned int * tbl);
void radix_find_range(int a,
                      const int * k1,
                      unsigned int * tbl,
                      unsigned int N,
                      unsigned int * R);
void linear_find_range(int x, const int * k1, R_xlen_t * R, const R_xlen_t N);
void ftc2(int * U0, int * U1, const int * k1, int N);

// maxmin
int maxXY(const int * x, const int * y, R_xlen_t Nx, R_xlen_t Ny, bool sx, bool sy);

int minii(int a, int b);
int mini3(int a, int b, int c);
int maxii(int a, int b);
int maxi3(int a, int b, int c);

double minid(int a, double b);
double maxid(int a, double b);

double mindd(double a, double b);
double maxdd(double a, double b);

double mind3(double a, double b, double c);
double maxd3(double a, double b, double c);

double Mind(const double * x, R_xlen_t N, int nThread);
double Maxd(const double * x, R_xlen_t N, int nThread);
int Maxi(const int * x, R_xlen_t N, int nThread);

// sortedness
bool sorted_int(const int * xp, R_xlen_t N, int nThreads);

#endif
