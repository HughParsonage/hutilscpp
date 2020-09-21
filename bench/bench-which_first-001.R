library(hutilscpp)
library(bench)

xi <- integer(1e8)
xd <- double(1e8)

bench::mark(
  which_first(xi > 0),
  which_first(xi > 0L),
  which_first(xd > 0),
  which_last(xi > 0)
)
