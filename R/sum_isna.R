#' Number of missing values
#' @description The count of missing values in an atomic vector, equivalent to
#' to \code{sum(is.na(x))}.
#' @param x An atomic vector.
#' @param do_anyNA Should \code{anyNA(x)} be executed before an attempt to
#' count the \code{NA}'s in \code{x} one-by-one? By default, set to \code{TRUE},
#' since it is generally quicker. It will only be slower when \code{NA} is rare
#' and occurs late in \code{x}.
#' @examples
#' sum_isna(c(1:5, NA))
#' @export

sum_isna <- function(x, do_anyNA = TRUE) {
  if (!is.atomic(x)) {
    stop("`x` was class ", paste0(class(x), collapse = " "), ", but must be atomic.")
  }
  if (length(x) == 0L) {
    return(0L)
  }
  if (do_anyNA && !anyNA(x)) {
    return(0L)
  }
  o <- switch(typeof(x),
              "logical" = sum_isna_logi(x),

              "integer" = sum_isna_int(x),

              "double"  = sum_isna_dbl(x),

              "complex" = sum_isna_complx(x),

              "character" = sum_isna_char(x),

              # nocov start
              {
                stop("Internal error: anyNA(x) was TRUE but typeof(x) is ",
                     typeof(x),
                     ", a contradiction.")
              }
              # nocov end
  )
  if (o < .Machine$integer.max) {
    o <- as.integer(o)
  }
  return(o)
}



