#ifndef hutilsc_H
#define hutilsc_H

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rversion.h>
#include <stdint.h> // for uint64_t rather than unsigned long long
#include <stdbool.h>
#include <math.h>

#if _OPENMP
#include <omp.h>
#endif

#if (__GNUC__ > 7) || \
((__GNUC__ == 7) && (__GNUC_MINOR__ > 3))
#define BUILTIN_ADD_OVERFLOW_EXIST
#endif

#define DEBUG 0

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

#define NA_INT -2147483648

extern int tens[10];

// allocate
SEXP LogicalN(R_xlen_t N);
SEXP IntegerN(R_xlen_t N);
SEXP IntegerNNA(R_xlen_t N);
SEXP DoubleN(R_xlen_t N);
SEXP DoubleNNA(R_xlen_t N);

// asInteger2
int asInteger2(SEXP x);
bool is_true(SEXP x);

// diagnose_omp
int as_nThread(SEXP x);
bool has_openmp();

SEXP ScalarLength(R_xlen_t o);

bool dsingle_ox_x1_x2(double x, int oix, double x1, double x2);
bool isingle_ox_x1_x2(int x, int oix, int x1, int x2);

bool do_is_safe2int(double x);
int dbl_is_int(double x);
int dbl2int(double x);
int sex2int1(SEXP x);

R_xlen_t sum_isna(SEXP x, SEXP nthreads) ;

// character
bool string_equaln(const char * x, int nx, const char * y);


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
void Vminmax_i(int minmax[], int * x, R_xlen_t N, int nthreads);

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

// sortedness
bool sorted_int(const int * xp, R_xlen_t N, int nThreads);

#endif
