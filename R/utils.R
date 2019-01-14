check_TF <- function(x) {
  if (is.logical(x) && length(x) == 1L) {
    if (anyNA(x)) {
      xc <- deparse(substitute(x))
      stop("`", xc, " = NA` but must be TRUE or FALSE. ",
           "Change `", xc, "` to be TRUE or FALSE.")
    } else {
      return(NULL)
    }
  } else {
    xc <- deparse(substitute(x))
    if (length(x) != 1L) {
      stop("`", xc, "` had length ", length(x), " but must be length-one. ",
           "Change `", xc, "` to be TRUE or FALSE.")
    } else {
      stop("`", xc, "` was type ", typeof(x), " but must be logical. ",
           "Change `", xc, "` to be TRUE or FALSE.")
    }
  }
}

isnt_number <- function(a, na.bad = TRUE, infinite.bad = TRUE) {
  if (!is.numeric(a)) {
    o <- TRUE
    ac <- deparse(substitute(a))
    attr(o, "ErrorMessage") <- paste0("`", ac, "` was a ", class(a), ", but must be numeric.")
    return(o)
  }
  if (length(a) != 1L) {
    o <- TRUE
    ac <- deparse(substitute(a))
    attr(o, "ErrorMessage") <- paste0("`", ac, "` had length ", length(a), ", but must be length-one.")
    return(o)
  }
  if (na.bad && is.na(a)) {
    o <- TRUE
    ac <- deparse(substitute(a))
    attr(o, "ErrorMessage") <- paste0("`", ac, "= NA`, but this is not permitted.")
    return(o)
  }
  if (infinite.bad && is.infinite(a)) {
    o <- TRUE
    ac <- deparse(substitute(a))
    attr(o, "ErrorMessage") <- paste0("`", ac, "` was not finite, but this is not permitted.")
    return(o)
  }
  FALSE
}

