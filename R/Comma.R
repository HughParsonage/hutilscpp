#' Faster version
#'
#' @param x A numeric vector.
#' @param digits An integer, similar to `round`.
#' @param big.mark A single character.
#'
#' @return
#' Similar to `prettyNum(round(x, digits), big.mark = ',')`.
#'
#' @export

Comma <- function(x, digits = 0L, big.mark = ',') {
  stopifnot(length(digits) == 1, !is.na(digits))
  if (digits < 0) {
    x <- as.integer(round(x, digits))
  }
  callow <- c(",", " ", "'", "_", "~", '"', "/")
  m <- match(big.mark,
             callow,
             nomatch = 0L)
  .Call("C_comma", x, digits, m,
        PACKAGE = "hutilscpp")
}

