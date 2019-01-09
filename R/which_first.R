#' Where does a logical expression first occur?
#' @param expr An expression, such as \code{x == 2}.
#' @return The same as \code{which.max(expr)} or \code{which(expr)[1]} but returns \code{0L}
#'  when \code{expr} has not \code{TRUE} values.
#'
#' @details
#' The performance benefit of using this function is when the RHS of
#' \code{expr} is a length-one numeric and the resultant logical
#' expression is lengthy and expensive.
#'
#' @examples
#' x <- rep_len(runif(1e4, 0, 6), 5e8)
#' bench_system_time(x > 5)
#' bench_system_time(which(x > 5))
#' bench_system_time(which.max(x > 5))
#' bench_system_time(which_first(x > 5))
#'
#' x <- double(1e8)
#' bench_system_time(x > 0)
#' bench_system_time(which(x > 0))
#' bench_system_time(which.max(x > 0))
#' bench_system_time(which_first(x > 0))
#'
#' @export


which_first <- function(expr) {
  rhs <- NULL
  sexpr <- substitute(expr)
  if (is.call(sexpr)) {
    operator <- as.character(sexpr[[1L]])
    lhs <- sexpr[[2L]]
    rhs <- sexpr[[3L]]
  }
  if (length(rhs) != 1L || !is.numeric(rhs)) {
    o <- which.max(expr)
    if (o == 1L && !expr[1L]) {
      o <- 0L
    }
    return(o)
  }
  lhs_eval <- eval.parent(lhs)
  switch(operator,
         "==" = {
           AnyWhich(lhs_eval, as.double(rhs), gt = FALSE, lt = FALSE, eq = TRUE)
         },
         "!=" = {
           AnyWhich(lhs_eval, as.double(rhs), gt = FALSE, lt = FALSE, eq = FALSE)
         },
         "<=" = {
           AnyWhich(lhs_eval, as.double(rhs), gt = FALSE, lt = TRUE, eq = TRUE)
         },
         "<" = {
           AnyWhich(lhs_eval, as.double(rhs), gt = FALSE, lt = TRUE, eq = FALSE)
         },
         ">=" = {
           AnyWhich(lhs_eval, as.double(rhs), gt = TRUE, lt = FALSE, eq = TRUE)
         },
         ">" = {
           AnyWhich(lhs_eval, as.double(rhs), gt = TRUE, lt = FALSE, eq = FALSE)
         },

         # Still proceed using base R
         {
           o <- which.max(expr)
           if (o == 1L && !expr[1L]) {
             o <- 0L
           }
           o
         })
}



