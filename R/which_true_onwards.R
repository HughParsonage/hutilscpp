#' At which point are all values true onwards
#' @param x A logical vector

which_true_onwards <- function(x) {
  if (!is.logical(x)) {
    stop("`x` was type ", class(x), ". `x` must be a logical vector.")
  }
  if (anyNA(x)) {
    stop("`x` had missing values. This is not permitted.")
  }

  do_which_true_onwards(x)
}
