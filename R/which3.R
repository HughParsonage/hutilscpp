#' which of three vectors are the elements (all, any) true?
#' @param x,y,z Logical vectors. Either the same length or length-1
#' @param And Boolean. If \code{TRUE}, only indices where all of x, y, z
#' are TRUE are returned; if \code{FALSE}, any index where x, y, z
#' are TRUE are returned.
#' @export

which3 <- function(x, y, z, And = TRUE) {
  stopifnot(is.logical(x), is.logical(y), is.logical(z))
  # if (length(x) != length(y)) {
  #   stop("`length(x) = ", length(x), ", yet ",
  #        "`length(y) = ", length(y), ".")
  # }
  # if (length(x) != length(z)) {
  #   stop("`length(x) = ", length(x), ", yet ",
  #        "`length(z) = ", length(z), ".")
  # }
  check_TF(And)
  # List produces [[1]] = maximum [[2]] result
  cpp_list <- do_which3(x, y, z, And)
  {cpp_list[[2]]}[seq_len(cpp_list[[1]])]
}



