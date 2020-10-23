Probable UBSAN error arising from doubles cast as ints now fixed by
special-casing NaN input in do_which_first__.

## Test environments
* local Windows 4.0.0
* ubuntu 18.04 (on travis-ci), R dev and release
* win-builder (devel and release)
* rhub with valgrind and sanitizers

## R CMD check results

0 errors | 0 warnings | 1 note

Note relates to recent submission. UBSAN error seemed urgent.
