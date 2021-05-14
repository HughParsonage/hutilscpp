
AnyCharMatch <- function(x, a, opposite = FALSE) {
  stopifnot(is.character(x), is.character(a))
  .Call("CAnyCharMatch", x, a, opposite, PACKAGE = packageName())
}
