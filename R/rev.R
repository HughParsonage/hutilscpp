

rev <- function(x) {
  # to avoid slowness on certain altrep vectors
  if (is_altrep(x)) {
    return(seq.int(x[length(x)], x[1]))
  }
  if (length(x)) x[length(x):1L] else x
}

