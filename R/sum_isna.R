#' Number of missing values
#' @description The count of missing values in an atomic vector, equivalent to
#' to \code{sum(is.na(x))}.
#' @param x An atomic vector.
#' @examples
#' sum_isna(c(1:5, NA))
#' @export

sum_isna <- function(x) {
  if (!is.atomic(x)) {
    stop("`x` was class ", paste0(class(x), collapse = " "), ", but must be atomic.")
  }
  if (!anyNA(x)) {
    return(0L)
  }
  switch(typeof(x),
         "logical" = {
           length(x) - sum(x, na.rm = TRUE) - sum(!x, na.rm = TRUE)
         },
         "integer" = {
           sum_isna_int(x)
         },
         "double" = {
           sum_isna_dbl(x)
         },
         "complex" = {

         },
         "character" = {
           sum_isna_char(x)
         },
         # nocov start
         {
           stop("Internal error: anyNA(x) was TRUE but typeof(x) is ",
                typeof(x),
                ", a contradiction.")
         }
         # nocov end
  )
}



