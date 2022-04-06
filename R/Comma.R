#' Faster version of \code{scales::comma}
#'
#' @param x A numeric vector.
#' @param digits An integer, similar to `round`.
#' @param big.mark A single character, the thousands separator.
#'
#' @return
#' Similar to `prettyNum(round(x, digits), big.mark = ',')` but rounds down
#' and \code{-1 < x < 0} will output \code{"-0"}.
#'
#' @export

Comma <- function(x, digits = 0L, big.mark = c(",", " ", "'", "_", "~", '"', "/")) {
  stopifnot(length(digits) == 1, !is.na(digits))
  if (digits < 0) {
    x <- as.integer(round(x, digits))
  }
  callow <- c(",", " ", "'", "_", "~", '"', "/")
  m <- match(big.mark,
             callow,
             nomatch = 0L)
  .Call("C_comma", x, digits, m[1],
        PACKAGE = "hutilscpp")
}

