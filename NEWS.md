## hutilscpp 0.11.0

### New features

- `mean_isna(x)` returns the proportion of `NA` in an atomic vector,
  equivalent to `mean(is.na(x))`. Thin wrapper over `sum_isna`.
- `and3s` / `or3s` (and `sum_and3s` / `sum_or3s`) gain three new
  arguments to make the dispatch contract explicit at the R boundary:
  `na`, `unsupported`, `recycle` (#45).
  - `na = "C"` (default) preserves the historical C-level mask
    behaviour, where missing values are interpreted by the C kernels
    without extra R-level NA handling. `na = "false"` is now the
    explicit two-valued filter-mask mode: when parsed predicate inputs
    contain `NA` / `NaN`, the wrapper evaluates the expression chain
    with base R and coerces missing predicate results to `FALSE`.
    `na = "base"` defers to base R's `&` / `|` whenever a parsed
    predicate value contains `NA`, so `NA` propagates.
  - `unsupported = "fallback"` (default) silently falls back to base
    `Reduce("&", ...)` / `Reduce("|", ...)` when the C kernel cannot
    handle a type/op/length combination. `unsupported = "error"` raises
    instead -- useful in tests so silent unsupported paths fail loudly.
  - `recycle = "base"` (default) matches base R's recycling rules
    (mismatched-length RHS goes through the fallback). `recycle =
    "strict"` errors before the kernel is called for any RHS length not
    in `{1, length(LHS), 2 for between}`.
- All three arguments default to the package's prior CRAN behaviour, so
  no existing user code is affected.

### Bug fixes

- `sum_isna()` now counts every missing value in ALTREP vectors instead of
  treating `anyNA(x)` as a count. This also fixes `mean_isna()` for ALTREP
  inputs with multiple missing values, such as deferred strings.
- Hardened `and3s` / `or3s` dispatch for issue #55 residuals: unsupported
  operators now fall back or error instead of silently no-oping, second-slot
  `%in%` / `%notin%` predicates are preprocessed before C dispatch, logical
  LHS comparisons with non-logical RHS fall back to base-compatible
  semantics, inverted `%]between[%` double paths return the documented empty
  mask, and `na = "base"` now rejects `type = "raw"` because raw masks cannot
  represent `NA`.

### Internal

- Phase 4 of the and3s/or3s refactor (epic #36). Centralises the
  pre-dispatch length check in `.validate_predicate_length`. The
  `unsupported` / `recycle` machinery shares the same fallback Reduce
  path the wrapper already uses.

## hutilscpp 0.10.12

### Performance

- The Phase 2 (#43) "every kernel uses `&=` / `|=`, mask init once"
  invariant added a wrapper-side `memset(ansp, 1/0, N)` plus a
  read-modify-write on the first predicate, which regressed multi-
  threaded `and3s` / `or3s` on long inputs (~+15-20% at N >= 1e8,
  nThread >= 4, where the package becomes memory-bandwidth bound).
  The first predicate is now dispatched in a new INIT mode: the
  kernel writes the predicate result directly without reading the
  mask, and the wrapper skips the memset entirely. Predicates 2+
  still go through the `&=` / `|=` invariant, so the position-
  asymmetry footgun Phase 2 retired stays retired.

  Benchmarked at N=1e9 against CRAN 0.10.10: `and3s(x > 5, x < 100)`
  is -3% (nT=1) and -8.5% (nT=4); `and3s(x > 5)` is -4% / -12%; every
  call shape tested is at-or-faster-than CRAN at both thread counts.

### Bug fixes

- `or3s(x %]between[% c(F, T))` for logical `x` now returns all-TRUE
  rather than all-FALSE. Pre-fix the kernel returned silently for the
  legacy LL OP_BC path, leaving the OR mask at its initial all-zero
  state -- contradicting the R-level `%]between[%` (which returns the
  complement of the open interval, all-TRUE for logical x in {0, 1}).
  Same path now memsets to 1, matching the R-level semantics.

- `and3s` / `or3s` with an integer `x` and a non-integer scalar `y`
  (e.g. `ix < 5.5`, `ix >= -1.5`) now reduce to the correct integer
  comparison via `floor(y)`. Previously the `y0` adjustment in
  `vand2s_ID` / `vor2s_ID` (M==1) gated on the sign of the truncated
  int, so several `(op, sign-of-y)` combinations produced wrong masks
  (and3s: OP_LT, OP_LE neg, OP_GE; or3s: subunit fractional `y` only).
  Surfaced by the Phase 1 grid harness (#49).
- `vand2s_II` M==2 fast path now handles `y0 == y1` for all three between
  operators. Previously only `%between%` was handled; `%(between)%` and
  `%]between[%` fell through to the general `y0 < y1` code, which gave
  wrong answers due to unsigned wrap-around in `betweeniiuu(x, y0+1, y0-1)`
  (#47).
- `and3s(...)` now honours `type = "raw"` / `type = "which"` when the
  C dispatcher returns NULL and execution falls back to base `&`.
  Previously an inner `return()` short-circuited the type conversion (#40).
- `vand2s_RI`/`vand2s_RD` no longer overwrite the running AND-chain mask
  when `%notin%` / `!=` is called against a scalar that is out of raw
  range (or, for `RD`, a non-integer or NaN). Always-true predicates are
  now correctly a no-op (#38, #39).
- Add missing `break;` after `OP_NI` in the M==N branches of
  `vand2s_RR` / `vand2s_RI` / `vand2s_RD`. Latent (the fall-through was
  semantically idempotent), but pinned for future refactors (#37).

### Documentation

- `?and3s` / `?or3s` now document the package's two-valued mask
  convention for `NA` / `NaN`-valued comparisons (treated as `FALSE`
  in the mask, except for `!=` / `%notin%` against `NaN` which remain
  `TRUE`). This is a divergence from base R, which propagates `NA`.

## hutilscpp 0.10.11

### Bug fixes

- `and3s` with a `%between%` term against a degenerate range
  (e.g. `c(5, 5)`, or fractional bounds that collapse to a single
  integer such as `c(4.5, 5.4)`) no longer overwrites the
  accumulating AND-chain in the integer fast path of `uc_betweenidd`.
- `and3s` with a `%between%`/`%(between)%`/`%]between[%` term where
  `x` is double and the bounds are integer no longer overwrites the
  accumulating AND-chain in the `M == 2` fast path of `vand2s_DI`.

## hutilscpp 0.10.10

- New unexported function `antiMode` for the least common element

### Internal

- Reduce tinytest verbosity to avoid excessive test logs. Thanks to CRAN for reporting.



### Internal

- Attempted resolution of a compiler backend error in the `Cminmax()` function with OpenMP on Clang19. 
  Updated array reduction logic for `RAW` inputs to improve compatibility and reliability.
- Fix UBSAN and non-API calls in fastmatch

## hutilscpp 0.10.6

### Bug fix

- `is_seq` (internal function in `finp`): fixed UB when `length(table) == 0`.

## hutilscpp 0.10.5

### Internal

- `STRING_PTR` changed to `STRING_PTR_RO` as required by new CRAN policies


## hutilscpp 0.10.4

### Internal

- `is_constant` does not inherit data.table multithreading

## hutilscpp 0.10.3

### Bug fixes:

* `and3s(rr == 0L)` works for raw `rr`

### Internal:

* Fix for -Wformat checks



## hutilscpp 0.10.2

* Fix for new is.atomic behaviour

## hutilscpp 0.10.0

* New features
  - `abs_diff` contains a which.max option = 3.



## hutilscpp 0.9.3

* Internal
  - Fix poor coding standards Wstrict-prototypes (`f()` now `f(void)`)

## hutilscpp 0.9.2

* Bug fixes:
  - `logical3`. When passes expression with non-numeric components, no longer 
    skips as if empty

## hutilscpp 0.9.1

* Bug fixes:
  - Fix UBSAN for Comma

## hutilscpp 0.9.0

* New functions:
  - `abs_diff` for non-allocating versions of `abs(x - y)`.
  - `character2integer` for a faster version of `as.integer(gsub("[^0-9]", "", x))`
  - `Comma`, relatedly, `prettyNum(x, big.mark = ",")`
  - `coalesce0` as a convenience function, equivalent to `coalesce(x, 0)` for correct type of 0.
  - `diam` and `thinner` for direct versions of `diff(minmax(x))`.
  - `every_int32` Returns a vector of every `integer`
  - `ModeC` most common element of integer vectors. 
  - `unique_fmatch` and `uniqueN_fmatch` for distinct elements.

* Internal changes
  - complex separation of expressions (`and3s` and friends) is now done using 
    a different logic, and performs internal logical operations on raw (`char`)
    vectors.

## hutilscpp 0.8.2

* Bug fixes:
  - sum_and/or is now more consistent at not summing NA values.

* New functions:
  - `allNA` equivalent to `all(is.na(x))`

## hutilscpp 0.8.1

* Bug fixes:
  - Implies returns correct results for `NA` results.
  
* Internal 
  - Header change for compatibility with clang13. Thanks to CRAN for reporting.

## hutilscpp 0.8.0

* Enhancements: 
  - `minmax` accepts raw input, treating as unsigned characters

* Internal:
  - A change to the `LOGICAL` C API has been absorbed.

## hutilscpp 0.7.2

* Bug fixes
  - Two programming errors detected by valgrind and rchk have been fixed

## hutilscpp 0.7.0

Functions are now in C to improve install time and size.

* New functions:
  - `Implies` for logical implies
  - `divisible2` test evenness of numbers
  - `fmatchp`, `finp` experimental parallel hashing functions
  - `is_sorted` and `isntSorted` for assertions about sorted atomic vectors
  - `minmax` multithreaded function of `c(min(x), max(x))`

## hutilscpp 0.5.2
* Attempt fix of UBSAN error in which_first


## hutilscpp 0.5.1

### Bug fixes:
* Reverses a performance regression in `which_first`, introduced in version 0.5.0, 
  caused by an overeliance on compiler optimization. (#20)
* OpenMP regions have been protected so that the package works without OpenMP
  


## hutilscpp 0.5.0

### Breaking changes
* `pminV` no longer accept non-numeric input
* `do_` functions have been removed entirely

### Bug fixes:
* `pmax0(x, in_place = TRUE)` now returns early, rather than checking the vector twice.
* `sum_isna` now reflects `sum(is.na(x))` when x contains `NaN`.
* `sum_isna` diverts ALTREP vectors to `anyNA` for performance and to avoid problems
   when passed to C++.

### New functions:
* `which_last` for the first index from the last index.
* `divisible` and `divisible16` for returning divisibility
* `count_logical` fast tabulation of logical vectors
* `and3s`, `or3s`, parallelized and _separated_ versions of `&`
* `sum_and3s` and `sum_or3s`, the sums of the above logical vectors. 
* `whichs` for an alternative implementation of `which` which separates the input
* `which_firstNA` and `which_lastNA` for first/last position of missing values

### Enhancements:
* `which_first` accepts argument `use.which.max` for better performance on known short inputs
* `is_constant` now accepts `nThread` for multithreaded checking of constant vectors
  and is much faster in general even in single-thread mode.
* `sum_isna` now accepts `nThread` for multithreaded accumulation of missing value counts
* `are_even` can be slightly faster on integers if ignoring `NA`, handles large 
  doubles (like `1e10`), and accepts `nThread`.
  



# hutilscpp 0.3.0

### Critical bug fixes:
* `is_safe2int(x)` now tolerates `NaN` input. Thanks to CRAN clang-UBSAN.

### Bug fixes:
* `which_first(x == y)` now works properly when `length(y) == length(x)`.


### New functions:
* `xor2` a faster version of `xor`. 

``` r
set.seed(1)
library(hutils)
library(hutilscpp)

bench__mark <- function(...) {
  dplyr::select(bench::mark(..., min_iterations = 12),
                expression, median, `itr/sec`, mem_alloc, n_gc)
}
x <- y <- logical(1e9)
bench__mark(xor(x, y), xor2(x, y))
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 x 5
#>   expression   median `itr/sec` mem_alloc  n_gc
#>   <chr>      <bch:tm>     <dbl> <bch:byt> <dbl>
#> 1 xor(x, y)    7.956s     0.126  14.901GB    16
#> 2 xor2(x, y)   1.652s     0.530   3.725GB     3
x <- !y
bench__mark(xor(x, y), xor2(x, y))
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 2 x 5
#>   expression   median `itr/sec` mem_alloc  n_gc
#>   <chr>      <bch:tm>     <dbl> <bch:byt> <dbl>
#> 1 xor(x, y)    8.227s     0.121  14.901GB    13
#> 2 xor2(x, y)   1.983s     0.460   3.725GB     3

x <- samp(c(TRUE, FALSE), 1e9)
y <- samp(c(TRUE, FALSE), 1e9)

bench__mark(xor(x, y), xor2(x, y))
#> # A tibble: 2 x 5
#>   expression   median `itr/sec` mem_alloc  n_gc
#>   <chr>      <bch:tm>     <dbl> <bch:byt> <dbl>
#> 1 xor(x, y)   20.276s    0.0493  14.901GB    11
#> 2 xor2(x, y)   1.971s    0.506    3.725GB     3

x <- samp(c(TRUE, FALSE, NA), 1e9)
y <- samp(c(TRUE, FALSE), 1e9)

benc__mark(xor(x, y), xor2(x, y))
#> # A tibble: 2 x 5
#>   expression   median `itr/sec` mem_alloc  n_gc
#>   <chr>      <bch:tm>     <dbl> <bch:byt> <dbl>
#> 1 xor(x, y)   25.063s    0.0399  14.901GB     2
#> 2 xor2(x, y)   4.524s    0.221    3.725GB     3
```

<sup>Created on 2019-08-25 by the [reprex package](https://reprex.tidyverse.org) (v0.3.0)</sup>



## hutilscpp 0.2.0

* Added a `NEWS.md` file to track changes to the package.

### Bug fixes:

* `which_first(x == y)` now supports logical `x` without returning arcane error messages.

### New functions:

* `is_constant`, for testing atomic vectors and `isntConstant` for the first
  different value
* `is_sorted` and `isntSorted` (currently private), similarly.
* `and3`, `or3` for ternary and/or enabling vectorized short-circuiting
* `sum_isna` for counting `NA` values.

### Enhancements

* `pminC` now handles integer inputs without coercing to double.
* `pmaxC(x, a)` accepts integer `a` when `x` is type double.
* `pmax0` and `pmin0` perform much better, especially when `x` is known and marked as sorted, but also
  due to a better algorithm using absolute value.
  
``` r
set.seed(1)
attach(asNamespace("hutilscpp"))
#> The following object is masked from package:base:
#> 
#>     isFALSE

bench__mark <- function(...) {
  dplyr::select(bench::mark(..., min_iterations = 12),
                expression, median, `itr/sec`, mem_alloc, n_gc)
}
x <- rep_len(rlnorm(1e6, 7, 2), 1e9)
bench__mark(do_pmaxC_dbl(x, 0), do_pmax0_abs_dbl(x))
#> # A tibble: 2 x 5
#>   expression              median `itr/sec`     mem_alloc  n_gc
#>   <chr>                 <bch:tm>     <dbl>     <bch:byt> <dbl>
#> 1 do_pmaxC_dbl(x, 0)  2428.139ms     0.405 3618205.211KB     4
#> 2 do_pmax0_abs_dbl(x)  777.362ms     1.28        6.539KB     0
x <- x - 1
bench__mark(do_pmaxC_dbl(x, 0), do_pmax0_abs_dbl(x))
#> # A tibble: 2 x 5
#>   expression            median `itr/sec` mem_alloc  n_gc
#>   <chr>               <bch:tm>     <dbl> <bch:byt> <dbl>
#> 1 do_pmaxC_dbl(x, 0)    2.394s     0.410   3.451GB     4
#> 2 do_pmax0_abs_dbl(x)   2.590s     0.386   3.451GB     4
x <- sort(x)
bench__mark(do_pmaxC_dbl(x, 0), do_pmax0_radix_sorted_dbl(x))
#> # A tibble: 2 x 5
#>   expression                     median `itr/sec` mem_alloc  n_gc
#>   <chr>                        <bch:tm>     <dbl> <bch:byt> <dbl>
#> 1 do_pmaxC_dbl(x, 0)             3.593s     0.313   6.901GB     5
#> 2 do_pmax0_radix_sorted_dbl(x)   2.306s     0.437   3.451GB     4

x <- rep_len(as.integer(rlnorm(1e6, 7, 2)), 1e9)
bench__mark(do_pmaxC_int(x, 0L), do_pmax0_abs_int(x))
#> # A tibble: 2 x 5
#>   expression              median `itr/sec`     mem_alloc  n_gc
#>   <chr>                 <bch:tm>     <dbl>     <bch:byt> <dbl>
#> 1 do_pmaxC_int(x, 0L) 2041.515ms     0.490 3906256.727KB     3
#> 2 do_pmax0_abs_int(x)  405.266ms     2.45        6.539KB     0
x <- x - 1L
bench__mark(do_pmaxC_int(x, 0L), do_pmax0_abs_int(x))
#> # A tibble: 2 x 5
#>   expression            median `itr/sec` mem_alloc  n_gc
#>   <chr>               <bch:tm>     <dbl> <bch:byt> <dbl>
#> 1 do_pmaxC_int(x, 0L)   1.449s     0.686   3.725GB     2
#> 2 do_pmax0_abs_int(x)   1.766s     0.577   3.725GB     1
x <- sort(x)
bench__mark(do_pmaxC_int(x, 0L), do_pmax0_radix_sorted_int(x))
#> # A tibble: 2 x 5
#>   expression                     median `itr/sec` mem_alloc  n_gc
#>   <chr>                        <bch:tm>     <dbl> <bch:byt> <dbl>
#> 1 do_pmaxC_int(x, 0L)            1.751s     0.568   7.451GB     2
#> 2 do_pmax0_radix_sorted_int(x)   1.404s     0.827   3.725GB     1
```

<sup>Created on 2019-08-10 by the [reprex package](https://reprex.tidyverse.org) (v0.3.0)</sup>
