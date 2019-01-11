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
#'
#' N <- 1e6
#' mem_lim <- memory.limit()
#' if (is.finite(mem_lim) && mem_lim > 32e3) {
#'   N <- 1e8
#' }
#'
#' x <- rep_len(runif(1e4, 0, 6), N)
#' bench_system_time(x > 5)
#' bench_system_time(which(x > 5))
#' bench_system_time(which.max(x > 5))
#' bench_system_time(which_first(x > 5))
#'
#' x <- double(N)
#' bench_system_time(x > 0)
#' bench_system_time(which(x > 0))
#' bench_system_time(which.max(x > 0))
#' bench_system_time(which_first(x > 0))
#'
#' @export


which_first <- function(expr) {
  rhs <- NULL
  sexpr <- substitute(expr)
  if (!is.call(sexpr) ||
      length(sexpr) != 3L ||
      anyNA(match(operator <- as.character(sexpr[[1L]]),
                  c("==", "<=", ">=", ">", "<", "!="))) ||
      !is.name(lhs <- sexpr[[2L]]) ||
      !is.numeric(rhs <- sexpr[[3L]])) {
    o <- which.max(expr)

    # o == 1L is wrong if all expr are FALSE
    if (o == 1L && !expr[1L]) {
      o <- 0L
    }
    return(o)
  }

  lhs_eval <- eval.parent(lhs)
  if (is.integer(lhs_eval) && operator == "==" && {is.integer(rhs) || rhs == as.integer(rhs)}) {
    return(match(as.integer(rhs), lhs_eval, nomatch = 0L))
  }


  if (is.character(lhs_eval)) {
    oc <-
      switch(operator,
             "==" = AnyCharMatch(lhs_eval, as.character(rhs)),
             {
               o <- which.max(expr)
               if (o == 1L && !expr[1L]) {
                 o <- 0L
               }
               o
             })
    return(oc)
  }

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

         # nocov start
         # Still proceed using base R
         {
           warning("Internal error: which_first:95")
           o <- which.max(expr)

           if (o == 1L && !expr[1L]) {
             o <- 0L
           }
           o
         }
         # nocov end
         )

}



