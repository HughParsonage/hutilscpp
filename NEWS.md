# hutilscpp 0.2.0

* Added a `NEWS.md` file to track changes to the package.

## Bug fixes:

* `which_first(x == y)` now supports logical `x` without returning arcane error messages.

## New functions:

* `is_constant`, for testing atomic vectors and `isntConstant` for the first
  different value
* `is_sorted` and `isntSorted` (currently private), similarly.
* `and3`, `or3` for ternary and/or enabling vectorized short-circuiting
* `sum_isna` for counting `NA` values.

## Enhancements

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


