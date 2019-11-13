clang-UBSAN issue has been fixed and tested. NaNs are caught before the casting to int.

## Test environments
* local Windows 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.6.1
* win-builder (devel and release)
* rhub with valgrind and sanitizers

## R CMD check results

0 errors | 0 warnings | 1 note

* I occasionally get a NOTE regarding spelling of neighbour. Spelling is UK.
