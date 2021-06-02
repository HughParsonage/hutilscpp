/*
 *  fastmatch: fast implementation of match() in R using semi-permanent hash tables
 *
 *  Copyright (C) 2010, 2011  Simon Urbanek
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; version 2 of the License.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 */
#include "hutilscpp.h"
/* hash_index_t is big enough to cover long vectors */
typedef R_xlen_t hash_index_t;

/* hashes are always 32-bit -- this is for compatibility with
 the hash function used in R.
 This means that long vectors are fine, but they may not have
 more than 2^32 - 1 unique values */
typedef unsigned int hash_value_t;

/* for malloc/free since we handle our hash table memory separately from R */
#include <stdlib.h>
#include <string.h>
/* for hashing for pointers we need intptr_t */
#include <stdint.h>


/* ".match.hash" symbol - cached on first use */
SEXP hs;

typedef struct hash {
  hash_index_t m, els; /* hash size, added elements (unused!) */
int k;               /* used bits */
SEXPTYPE type;       /* payload type */
void *src;           /* the data array of the hashed object */
SEXP prot;           /* object to protect along whith this hash */
SEXP parent;         /* hashed object */
struct hash *next;   /* next hash table - typically for another type */
hash_index_t ix[1];  /* actual table of indices */
} hash_t;

/* create a new hash table with the given source and length.
 we store only the index - values are picked from the source
 so you must make sure the source is still alive when used */
static hash_t *new_hash(void *src, hash_index_t len) {
  hash_t *h;
  int k = 1;
  hash_index_t m = 2, desired = len * 2; /* we want a maximal load of 50% */
while (m < desired) { m *= 2; k++; }
h = (hash_t*) calloc(1, sizeof(hash_t) + (sizeof(hash_index_t) * m));
if (!h) Rf_error("unable to allocate %.2fMb for a hash table", (double) sizeof(hash_index_t) * (double) m / (1024.0 * 1024.0));
h->m = m;
h->k = k;
h->src = src;
return h;
}

/* free the hash table (and all chained hash tables as well) */
static void free_hash(hash_t *h) {
  if (h->next) free_hash(h->next);
  if (h->prot) R_ReleaseObject(h->prot);
  free(h);
}

/* R finalized for the hash table object */
static void hash_fin(SEXP ho) {
  hash_t *h = (hash_t*) EXTPTR_PTR(ho);
  if (h) free_hash(h);
}

/* pi-hash fn */
#define HASH(X) (3141592653U * ((unsigned int)(X)) >> (32 - h->k))

/* add the integer value at index i (0-based!) to the hash */
static hash_value_t add_hash_int(hash_t *h, hash_index_t i) {
  int *src = (int*) h->src;
  int val = src[i++];
  hash_value_t addr = HASH(val);
#ifdef PROFILE_HASH
  hash_value_t oa = addr;
#endif
  while (h->ix[addr] && src[h->ix[addr] - 1] != val) {
    addr++;
    if (addr == h->m) addr = 0;
  }
#ifdef PROFILE_HASH
  if (addr != oa) Rprintf("%d: dist=%d (addr=%d, oa=%d)\n", val,
      (int) (addr - oa), (int) addr, (int) oa);
#endif
  if (!h->ix[addr])
    h->ix[addr] = i;
  return addr;
}

/* to avoid aliasing rules issues use a union */
union dint_u {
  double d;
  unsigned int u[2];
};

/* add the double value at index i (0-based!) to the hash */
static hash_value_t add_hash_real(hash_t *h, hash_index_t i) {
  double *src = (double*) h->src;
  union dint_u val;
  hash_value_t addr;
  /* double is a bit tricky - we nave to nomalize 0.0, NA and NaN */
  val.d = (src[i] == 0.0) ? 0.0 : src[i];
  if (R_IsNA(val.d)) val.d = NA_REAL;
  else if (R_IsNaN(val.d)) val.d = R_NaN;
  addr = HASH(val.u[0]+ val.u[1]);
#ifdef PROFILE_HASH
  hash_value_t oa = addr;
#endif
  while (h->ix[addr] && src[h->ix[addr] - 1] != val.d) {
    addr++;
    if (addr == h->m) addr = 0;
  }
#ifdef PROFILE_HASH
  if (addr != oa)
    Rprintf("%g: dist=%d (addr=%d, oa=%d)\n", val.d,
            (int) (addr - oa), (int)addr, (int)oa);
#endif
  if (!h->ix[addr])
    h->ix[addr] = i + 1;
  return addr;
}

/* add the pointer value at index i (0-based!) to the hash */
static int add_hash_ptr(hash_t *h, hash_index_t i) {
  hash_value_t addr;
  void **src = (void**) h->src;
  intptr_t val = (intptr_t) src[i++];
#if (defined _LP64) || (defined __LP64__) || (defined WIN64)
  addr = HASH((val & 0xffffffff) ^ (val >> 32));
#else
  addr = HASH(val);
#endif
#ifdef PROFILE_HASH
  hash_value_t oa = addr;
#endif
  while (h->ix[addr] && (intptr_t) src[h->ix[addr] - 1] != val) {
    addr++;
    if (addr == h->m) addr = 0;
  }
#ifdef PROFILE_HASH
  if (addr != oa)
    Rprintf("%p: dist=%d (addr=%d, oa=%d)\n", val,
            (int)(addr - oa), (int)addr, (int)oa);
#endif
  if (!h->ix[addr])
    h->ix[addr] = i;
  return addr;
}

/* NOTE: we are returning a 1-based index ! */
static hash_index_t get_hash_int(hash_t *h, int val, int nmv) {
  int *src = (int*) h->src;
  hash_value_t addr = HASH(val);
  while (h->ix[addr]) {
    if (src[h->ix[addr] - 1] == val)
      return h->ix[addr];
    addr++;
    if (addr == h->m) addr = 0;
  }
  return nmv;
}

/* NOTE: we are returning a 1-based index ! */
static hash_index_t get_hash_real(hash_t *h, double val, int nmv) {
  double *src = (double*) h->src;
  hash_value_t addr;
  union dint_u val_u;
  /* double is a bit tricky - we nave to normalize 0.0, NA and NaN */
  if (val == 0.0) val = 0.0;
  if (R_IsNA(val)) val = NA_REAL;
  else if (R_IsNaN(val)) val = R_NaN;
  val_u.d = val;
  addr = HASH(val_u.u[0] + val_u.u[1]);
  while (h->ix[addr]) {
    if (!memcmp(&src[h->ix[addr] - 1], &val, sizeof(val)))
      return h->ix[addr];
    addr++;
    if (addr == h->m) addr = 0;
  }
  return nmv;
}

/* NOTE: we are returning a 1-based index ! */
static hash_index_t get_hash_ptr(hash_t *h, void *val_ptr, int nmv) {
  void **src = (void **) h->src;
  intptr_t val = (intptr_t) val_ptr;
  hash_value_t addr;
#if (defined _LP64) || (defined __LP64__) || (defined WIN64)
  addr = HASH((val & 0xffffffff) ^ (val >> 32));
#else
  addr = HASH(val);
#endif
  while (h->ix[addr]) {
    if ((intptr_t) src[h->ix[addr] - 1] == val)
      return h->ix[addr];
    addr ++;
    if (addr == h->m) addr = 0;
  }
  return nmv;
}

static SEXP asCharacter(SEXP s, SEXP env)
{
  SEXP call, r;
  PROTECT(call = lang2(install("as.character"), s));
  r = eval(call, env);
  UNPROTECT(1);
  return r;
}

// this function is only used with long vectors
// # nocov start
static double NA_int2real(hash_index_t res) {
  return (res == NA_INTEGER) ? R_NaReal : ((double)  res);
}
// # nocov end

/* the only externally visible function to be called from R */
SEXP fmatch(SEXP x, SEXP y, SEXP nonmatch, SEXP Fin, SEXP WhichFirst, SEXP nthreads) {
  int nThread = asInteger(nthreads);
  if (TYPEOF(WhichFirst) != INTSXP ||
      xlength(WhichFirst) != 1) {
    error("Internal error(fmatch): WhichFirst not int."); // # nocov
  }
  const int whichfirst = asInteger(WhichFirst);
  const bool fin = asLogical(Fin);
  SEXP a;
  SEXPTYPE type;
  hash_t *h = 0;
  int nmv = asInteger(nonmatch), np = 0, y_to_char = 0, y_factor = 0;
  if (fin | whichfirst) {
    nmv = 0; // fin should cause 0 in result
  }
  hash_index_t n = XLENGTH(x);

  /* edge-cases of 0 length */
  if (n == 0) {
    if (whichfirst) {
      return ScalarLength(0);
    }
    return allocVector(fin ? LGLSXP : INTSXP, 0);
  }
  if (XLENGTH(y) == 0) {
    if (whichfirst) {
      return ScalarLength(0);
    }
    if (fin) {
      return LogicalN(n);
    }
    // empty table -> vector full of nmv

    hash_index_t ii;
    a = PROTECT(allocVector(INTSXP, n));
    ++np;
    int * ai = INTEGER(a);
    for (ii = 0; ii < n; ii++) ai[ii] = nmv;
    if (np) UNPROTECT(np);
    return a;
  }

  /* implicitly convert factors/POSIXlt to character */
  if (OBJECT(x)) {
    if (inherits(x, "factor")) {
      x = PROTECT(asCharacterFactor(x));
      np++;
    } else if (inherits(x, "POSIXlt")) {
      x = PROTECT(asCharacter(x, R_GlobalEnv)); /* FIXME: match() uses env properly - should we switch to .External ? */
  np++;
    }
  }

  /* for y we may need to do that later */
  y_factor = OBJECT(y) && inherits(y, "factor");
  y_to_char = y_factor || (OBJECT(y) && inherits(y, "POSIXlt"));

  /* coerce to common type - in the order of SEXP types */
  if(TYPEOF(x) >= STRSXP || TYPEOF(y) >= STRSXP)
    type = STRSXP;
  else
    type = (TYPEOF(x) < TYPEOF(y)) ? TYPEOF(y) : TYPEOF(x);

  /* we only support INT/REAL/STR */
  if (type != INTSXP && type != REALSXP && type != STRSXP) {
    if (np) UNPROTECT(np); // # nocov
    return R_NilValue; // # nocov
  }

  if (y_to_char && type != STRSXP) /* y = factor -> character -> type must be STRSXP */
  type = STRSXP; // # nocov

  /* coerce x - not y yet because we may get away with the existing cache */
  if (TYPEOF(x) != type) {
    x = PROTECT(coerceVector(x, type));
    np++;
  }

  /* find existing cache(s) */
  if (!hs) hs = Rf_install(".match.hash");
  a = Rf_getAttrib(y, hs);
  if (a != R_NilValue) { /* if there is a cache, try to find the matching type */
  h = (hash_t*) EXTPTR_PTR(a);
    /* could the object be out of sync ? If so, better remove the hash and ignore it */
    if (!h || h->parent != y) {
#if HASH_VERBOSE
      Rprintf(" - DISCARDING hash, its parent and the bearer don't match, taking no chances.\n");
#endif
      h = 0;
      Rf_setAttrib(y, hs, R_NilValue);
    }
    while (h && h->type != type) h = h->next;
  }
  /* if there is no cache or not of the needed coerced type, create one */
  if (a == R_NilValue || !h) {
    h = new_hash(DATAPTR(y), XLENGTH(y));
    h->type = type;
    h->parent = y;
#if HASH_VERBOSE
    Rprintf(" - creating new hash for type %d\n", type);
#endif
    if (a == R_NilValue || !EXTPTR_PTR(a)) { /* if there is no cache attribute, create one */
  a = R_MakeExternalPtr(h, R_NilValue, R_NilValue);
      Rf_setAttrib(y, hs, a);
      Rf_setAttrib(a, R_ClassSymbol, Rf_mkString("match.hash"));
      R_RegisterCFinalizer(a, hash_fin);
    } else { /* otherwise append the new cache */
  hash_t *lh = (hash_t*) EXTPTR_PTR(a);
      while (lh->next) lh = lh->next;
      lh->next = h;
#if HASH_VERBOSE
      Rprintf("   (appended to the cache list)\n");
#endif
    }

    if (TYPEOF(y) != type) {
#if HASH_VERBOSE
      if (y_to_char)
        Rprintf("   (need to convert table factor/POSIXlt to strings\n");
      else
        Rprintf("   (need to coerce table to %d)\n", type);
#endif
      y = y_to_char ? (y_factor ? asCharacterFactor(y) : asCharacter(y, R_GlobalEnv)) : coerceVector(y, type);
      h->src = DATAPTR(y); /* this is ugly, but we need to adjust the source since we changed it */
  h->prot = y; /* since the coerced object is temporary, we let the hash table handle its life span */
  R_PreserveObject(y);
    }
    /* make sure y doesn't go away while we create the hash */
    /* R_PreserveObject(y);     */
    /* spawn a thread to create the hash */
    /* nope - so far we do it serially */

    { /* create the hash table */
    hash_index_t i, n = XLENGTH(y);
      if (type == INTSXP)
        for(i = 0; i < n; i++)
          add_hash_int(h, i);
      else if (type == REALSXP)
        for(i = 0; i < n; i++)
          add_hash_real(h, i);
      else
        for(i = 0; i < n; i++)
          add_hash_ptr(h, i);
    }
  }

  if (fin) {
    R_xlen_t i = 0, n = xlength(x);
    SEXP r = PROTECT(allocVector(LGLSXP, n));
    ++np;
    int * v = LOGICAL(r);
    if (type == INTSXP) {
      int *k = INTEGER(x);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (i = 0; i < n; i++) {
        v[i] = get_hash_int(h, k[i], nmv) != 0;
      }
    } else if (type == REALSXP) {
      double *k = REAL(x);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
      for (i = 0; i < n; i++) {
        v[i] = get_hash_real(h, k[i], nmv) != 0;
      }
    } else {
      SEXP *k = (SEXP*) DATAPTR(x);
      for (i = 0; i < n; i++)
        v[i] = get_hash_ptr(h, k[i], nmv) != 0;
    }
    if (np) UNPROTECT(np);
    return r;
  }

  if (whichfirst) {
    R_xlen_t wo = 0;
    if (whichfirst > 0) {
      // which_first
      R_xlen_t n = xlength(x);
      if (type == INTSXP) {
        int *k = INTEGER(x);
        for (R_xlen_t i = 0; i < n; i++) {
          if (get_hash_int(h, k[i], nmv) != 0) {
            wo = i + 1;
            break;
          }
        }
      } else if (type == REALSXP) {
        double *k = REAL(x);
        for (R_xlen_t i = 0; i < n; i++) {
          if (get_hash_real(h, k[i], nmv) != 0) {
            wo = i + 1;
            break;
          }
        }
      } else {
        SEXP *k = (SEXP*) DATAPTR(x);
        for (R_xlen_t i = 0; i < n; i++) {
          if (get_hash_ptr(h, k[i], nmv) != 0) {
            wo = i + 1;
            break;
          }
        }
      }
    } else {
      // which_last
      R_xlen_t n = xlength(x);
      if (type == INTSXP) {
        int *k = INTEGER(x);
        for (R_xlen_t i = n - 1; i >= 0; i--) {
          if (get_hash_int(h, k[i], nmv) != 0) {
            wo = i + 1;
            break;
          }
        }
      } else if (type == REALSXP) {
        double *k = REAL(x);
        for (R_xlen_t i = n - 1; i >= 0; i--) {
          if (get_hash_real(h, k[i], nmv) != 0) {
            wo = i + 1;
            break;
          }
        }
      } else {
        SEXP *k = (SEXP*) DATAPTR(x);
        for (R_xlen_t i = n - 1; i >= 0; i--) {
          if (get_hash_ptr(h, k[i], nmv) != 0) {
            wo = i + 1;
            break;
          }
        }
      }
    }
    if (np) UNPROTECT(np);
    return ScalarLength(wo);
  }



  // # nocov start
  if (xlength(x) >= INT_MAX) {
    hash_index_t i, n = XLENGTH(x);
    SEXP r = PROTECT(allocVector(REALSXP, n));
    ++np;
    double *v = REAL(r);
    if (nmv == NA_INTEGER) {
      /* we have to treat nmv = NA differently,
       because is has to be transformed into
       NA_REAL in the result. To avoid checking
       when nmv is different, we have two paths */
      if (type == INTSXP) {
        int *k = INTEGER(x);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
        for (i = 0; i < n; i++) {
          v[i] = NA_int2real(get_hash_int(h, k[i], NA_INTEGER));
        }
      } else if (type == REALSXP) {
        double *k = REAL(x);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
        for (i = 0; i < n; i++) {
          v[i] = NA_int2real(get_hash_real(h, k[i], NA_INTEGER));
        }
      } else {
        SEXP *k = (SEXP*) DATAPTR(x);
        for (i = 0; i < n; i++)
          v[i] = NA_int2real(get_hash_ptr(h, k[i], NA_INTEGER));
      }
    } else { /* no need to transcode nmv */
      if (type == INTSXP) {
        int *k = INTEGER(x);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
        for (i = 0; i < n; i++) {
          v[i] = (double) get_hash_int(h, k[i], nmv);
        }
      } else if (type == REALSXP) {
        double *k = REAL(x);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
        for (i = 0; i < n; i++) {
          v[i] = (double) get_hash_real(h, k[i], nmv);
        }
      } else {
        SEXP *k = (SEXP*) DATAPTR(x);
        for (i = 0; i < n; i++)
          v[i] = (double) get_hash_ptr(h, k[i], nmv);
      }
    }
    if (np) UNPROTECT(np);
    return r;
  }
  // # nocov end
  /* short vector - everything is int */
  R_xlen_t N = xlength(x);
  SEXP r = PROTECT(allocVector(INTSXP, N));
  ++np;
  int *v = INTEGER(r);
  if (type == INTSXP) {
    int *k = INTEGER(x);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < N; i++) {
      v[i] = get_hash_int(h, k[i], nmv);
    }
  } else if (type == REALSXP) {
    double *k = REAL(x);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
    for (R_xlen_t i = 0; i < N; i++) {
      v[i] = get_hash_real(h, k[i], nmv);
    }
  } else {
    SEXP *k = (SEXP*) DATAPTR(x);
    for (R_xlen_t i = 0; i < N; i++)
      v[i] = get_hash_ptr(h, k[i], nmv);
  }
  UNPROTECT(np);
  return r;
}



