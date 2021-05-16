#ifndef hutilsc_H
#define hutilsc_H

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);
#include <stdint.h> // for uint64_t rather than unsigned long long
#include <stdbool.h>

#if _OPENMP
#include <omp.h>
#endif

#if (__GNUC__ > 7) || \
((__GNUC__ == 7) && (__GNUC_MINOR__ > 3))
#define BUILTIN_ADD_OVERFLOW_EXIST
#endif

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

bool dsingle_ox_x1_x2(double x, int oix, double x1, double x2);
bool isingle_ox_x1_x2(int x, int oix, int x1, int x2);

#endif
