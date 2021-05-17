#' Implies
#' @param x,y Logical vectors of equal length.
#' @param anyNAx,anyNAy Whether \code{x,y} may contain \code{NA}.
#' If \code{FALSE}, the function runs faster, but under that assumption.
#'
#' @return Logical implies: \code{TRUE} unless \code{x} is \code{TRUE} and \code{y} is \code{FALSE}.
#'
#' \code{NA} in either \code{x} or \code{y} results in \code{NA} if and only if the result is unknown.
#' In particular \code{NA \%implies\% TRUE} is \code{TRUE} and \code{FALSE \%implies\% NA} is \code{TRUE}.
#'
#' If \code{x} or \code{y} are length-one, the function proceeds as if the length-one vector were recycled
#' to the length of the other.
#'
#'
#'
#' @export Implies
#'
#' @examples
#' library(data.table)
#' CJ(x = c(TRUE,
#'          FALSE),
#'    y = c(TRUE,
#'          FALSE))[, ` x => y` := Implies(x, y)][]
#'
#' #>        x     y  x => y
#' #> 1: FALSE FALSE    TRUE
#' #> 2: FALSE  TRUE    TRUE
#' #> 3:  TRUE FALSE   FALSE
#' #> 4:  TRUE  TRUE    TRUE
#'
#' # NA results:
#' #> 5:    NA    NA      NA
#' #> 6:    NA FALSE      NA
#' #> 7:    NA  TRUE    TRUE
#' #> 8: FALSE    NA    TRUE
#' #> 9:  TRUE    NA      NA

Implies <- function(x, y, anyNAx = TRUE, anyNAy = TRUE) {
  stopifnot(length(x) == length(y),
            is.logical(x),
            is.logical(y))
  check_TF(anyNAx)
  check_TF(anyNAy)
  .Call("CImplies", x, y, anyNAx, anyNAy, PACKAGE = packageName())
}
