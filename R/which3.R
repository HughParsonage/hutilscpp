#' which of three vectors are the elements (all, any) true?
#' @param x,y,z Logical vectors. Either the same length or length-1
#' @param And Boolean. If \code{TRUE}, only indices where all of x, y, z
#' are TRUE are returned; if \code{FALSE}, any index where x, y, z
#' are TRUE are returned.
#' @param prepare (logical, default: \code{FALSE})
#' Create \code{which(x)}, \code{which(y)}, \code{which(z)}
#' then determine the intersection.
#' @export

which3 <- function(x, y, z, And = TRUE, prepare = FALSE) {
  stopifnot(is.logical(x), is.logical(y), is.logical(z))
  if (isTRUE(prepare)) {
    wx <- which(x)
    wy <- which(y)
    wz <- which(z)

    o <-
      switch(which.min(c(length(wx), length(wy), length(wz))),
             # x is longest
             if (length(wy) >= length(wz)) {
               do_which3_prepare(wx, wy, wz)
             } else {
               do_which3_prepare(wx, wz, wy)
             },

             # y is longest
             if (length(wx) >= length(wz)) {
               do_which3_prepare(wy, wx, wz)
             } else {
               do_which3_prepare(wy, wz, wx)
             },

             # z is longest
             if (length(wx) >= length(wy)) {
               do_which3_prepare(wz, wx, wy)
             } else {
               do_which3_prepare(wz, wy, wx)
             })
    return(o)
  }
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



