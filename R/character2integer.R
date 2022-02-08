#' Character to numeric
#' @param x A character vector.
#' @param na.strings A set of strings that shall be coerced to \code{NA_integer_}
#' @param allow.double \code{logical(1)} If \code{TRUE}, a double vector may be returned.
#' If \code{FALSE}, an error will be emitted. If \code{NA}, numeric values outside
#' integer range are coerced to \code{NA_integer_}, silently.
#' @param option Control behaviour:
#' \describe{
#' \item{0}{Strip commas.}
#' }
#'
#' @export
character2integer <- function(x,
                              na.strings = NULL,
                              allow.double = FALSE,
                              option = 0L) {
  .Call("C_character2integer", x, na.strings, allow.double, option, PACKAGE = "hutilscpp")
}



