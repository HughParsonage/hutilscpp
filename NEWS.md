# hutilscpp 0.2.0

* Added a `NEWS.md` file to track changes to the package.

## Bug fixes:

* `which_first(x == y)` now supports logical `x` without returning arcane error messages.

## New functions:

* `is_constant`, for testing atomic vectors
* `and3`, `or3` for ternary and/or enabling vectorized short-circuiting

## Enhancements

* `pminC` now handles integer inputs without coercing to double

