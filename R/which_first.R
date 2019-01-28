#' Where does a logical expression first return \code{TRUE}?
#' @description A faster and safer version of \code{which.max} applied
#' to simple to parse logical expressions.
#'
#' @param expr An expression, such as \code{x == 2}.
#' @return The same as \code{which.max(expr)} or \code{which(expr)[1]} but returns \code{0L}
#'  when \code{expr} has no \code{TRUE} values.
#'
#' @details
#' If the \code{expr} is of the form \code{LHS <operator> RHS}
#' and \code{LHS} is a single symbol, \code{operator} is one of
#' \code{==},  \code{!=}, \code{>}, \code{>=}, \code{<}, \code{<=},
#' or \code{\%in\%}.
#' and \code{RHS} is a single numeric value, then \code{expr} is not
#' evaluated directed; instead each element of \code{LHS} is compared
#' individually.
#'
#' If \code{expr} is not of the above form, then \code{expr} is evaluated
#' and passed to \code{which.max}.
#'
#' Using this function can be significantly faster when the computation
#' of \code{expr} would be expensive. (The difference is only likely to
#' be clear when \code{length(x)} is much larger than 10 million.)
#' But even for smaller vectors, it has the benefit of returning
#' \code{0L} if none of the values in \code{expr} are \code{TRUE}, unlike
#' \code{which.max}.
#'
#'
#' @examples
#'
#' N <- 1e5
#' # N <- 1e8  ## too slow for CRAN
#'
#' # Two examples, from slowest to fastest,
#' # run with N = 1e8 elements
#'
#'                                        # seconds
#' x <- rep_len(runif(1e4, 0, 6), N)
#' bench_system_time(x > 5)
#' bench_system_time(which(x > 5))        # 0.8
#' bench_system_time(which.max(x > 5))    # 0.3
#' bench_system_time(which_first(x > 5))  # 0.000
#'
#' ## Worst case: have to check all N elements
#' x <- double(N)
#' bench_system_time(x > 0)
#' bench_system_time(which(x > 0))        # 1.0
#' bench_system_time(which.max(x > 0))    # 0.4  but returns 1, not 0
#' bench_system_time(which_first(x > 0))  # 0.1
#'
#' x <- as.character(x)
#' # bench_system_time(which(x == 5))     # 2.2
#' bench_system_time(which.max(x == 5))   # 1.6
#' bench_system_time(which_first(x == 5)) # 1.3
#'
#' @export


which_first <- function(expr) {
  rhs <- NULL
  sexpr <- substitute(expr)
  if (!is.call(sexpr) ||
      length(sexpr) != 3L ||
      anyNA(match(operator <- as.character(sexpr[[1L]]),
                  c("==", "<=", ">=", ">", "<", "!=", "%in%"))) ||
      !is.name(lhs <- sexpr[[2L]]) ||
      NOR(is.numeric(rhs <- sexpr[[3L]]),
          AND(operator == "%in%",
              # c(0, 1, 2) is not numeric but it is when evaluated
              is.numeric(rhs_eval <- eval.parent(rhs))))) {
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
         "%in%" = {
           if (is.integer(lhs_eval)) {
             AnyWhichInInt(lhs_eval, as.integer(rhs_eval))
           } else {
             AnyWhichInDbl(lhs_eval, as.double(rhs_eval))
           }
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



