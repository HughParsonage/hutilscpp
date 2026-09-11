# Raw membership must not coerce a fractional table to raw (5.5 -> 05).
local({
  x <- rep(as.raw(c(0, 5, 6, 255)), 251L)
  false <- rep(FALSE, length(x))
  for (table in list(5.5, c(5.5, 6.5, 255.5), c(5, 6.5),
                     c(-1, 256, 5.5), c(NA_real_, 5.5), numeric())) {
    for (op in c("%in%", "%notin%")) {
      # The raw kernels compare byte values numerically to numeric tables.
      expected <- as.integer(x) %in% table
      if (op == "%notin%") expected <- !expected
      for (na in c("C", "false", "base")) {
        for (slot in 1:2) {
          predicate <- as.call(list(as.name(op), quote(x), quote(table)))
          call <- if (slot == 1L) substitute(or3s(PRED, false, na = NA_MODE,
                                                 recycle = "strict"),
                                             list(PRED = predicate, NA_MODE = na)) else {
            substitute(or3s(false, PRED, na = NA_MODE, recycle = "strict"),
                       list(PRED = predicate, NA_MODE = na))
          }
          expect_identical(eval(call), expected)
        }
      }
    }
  }
})
