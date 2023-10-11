#' Indices of truthy raw values
#' @param x A raw vector. Other vectors are converted to logical if nonzero.
#' @return
#' which(x != as.raw(0))
#' @noRd

which_raw <- function(x) {
  .Call("C_which_raw", x, getOption("hutilscpp.nThread", 10L), PACKAGE = "hutilscpp")
}


