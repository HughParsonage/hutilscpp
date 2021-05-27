

vname <- function(x) {
  # from package:checkmate, Michel Lang
  paste0(deparse(eval.parent(substitute(substitute(x))), width.cutoff = 500L),
         collapse = "\n")
}
