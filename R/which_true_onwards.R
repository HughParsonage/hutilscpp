#' At which point are all values true onwards
#' @param x A logical vector

which_true_onwards <- function(x) {
  stopifnot(is.logical(x), anyNA(x))
  do_which_true_onwards(x)
}
