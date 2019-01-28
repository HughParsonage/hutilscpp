#' Where does a logical expression first return \code{TRUE}?
#' @description A faster and safer version of \code{which.max} applied
#' to simple-to-parse logical expressions.
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
#' evaluated directly; instead, each element of \code{LHS} is compared
#' individually.
#'
#' If \code{expr} is not of the above form, then \code{expr} is evaluated
#' and passed to \code{which.max}.
#'
#' Using this function can be significantly faster than the alternatives
#' when the computation
#' of \code{expr} would be expensive, though the difference is only likely to
#' be clear when \code{length(x)} is much larger than 10 million.
#' But even for smaller vectors, it has the benefit of returning
#' \code{0L} if none of the values in \code{expr} are \code{TRUE}, unlike
#' \code{which.max}.
#'
#' Compared to \code{\link[base:funprog]{Position}} for an appropriate
#'  choice of \code{f} the speed of \code{which_first} is not much faster
#'  when the expression is \code{TRUE} for some position. However, \code{which_first}
#'  is faster when all elements of \code{expr} are \code{FALSE}.
#'  Thus \code{which_first} has a smaller worst-case time than the
#'  alternatives for most \code{x}.
#'
#'
#'
#'
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
          AND(# c(0, 1, 2) is not numeric but it is when evaluated
              is.numeric(eval.parent(rhs)),
              operator == "%in%"))) {
    o <- which.max(expr)

    # o == 1L is wrong if all expr are FALSE
    if (o == 1L && !expr[1L]) {
      o <- 0L
    }
    return(o)
  }

  lhs_eval <- eval.parent(lhs)
  rhs_eval <- eval.parent(rhs)


  if (is.character(lhs_eval)) {
    oc <-
      switch(operator,
             "==" = AnyCharMatch(lhs_eval, as.character(rhs_eval)),
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
           if (is.double(lhs_eval)) {
             o <- AnyWhich_dbl(lhs_eval, as.double(rhs_eval), gt = FALSE, lt = FALSE, eq = TRUE)
           }
           if (is.integer(lhs_eval)) {
             # Need to pass int to Rcpp, but 2 != 2.5
             if (is.double(rhs_eval) && as.integer(rhs_eval) != rhs_eval) {
               # if rhs isn't even an integer, then
               # the first element of any integer vector
               # will not be equal to it.
               return(0L)
             }
             o <- AnyWhich_int(lhs_eval, as.integer(rhs_eval), gt = FALSE, lt = FALSE, eq = TRUE)
           }
         },
         "!=" = {
           if (is.double(lhs_eval)) {
             o <- AnyWhich_dbl(lhs_eval, as.double(rhs_eval), gt = FALSE, lt = FALSE, eq = FALSE)
           }
           if (is.integer(lhs_eval)) {
             if (is.double(rhs_eval) && as.integer(rhs_eval) != rhs_eval) {
               # Like ==, if rhs isn't even an integer, then
               # the first element of any integer vector
               # will not be equal to it. But if lhs_eval
               # has no length, we should return 0L
               if (!length(lhs_eval)) {
                 return(0L)
               } else {
                 return(1L)
               }
             }
             o <- AnyWhich_int(lhs_eval, as.integer(rhs_eval), gt = FALSE, lt = FALSE, eq = FALSE)
           }
         },
         "<=" = {
           if (is.double(lhs_eval)) {
             o <- AnyWhich_dbl(lhs_eval, as.double(rhs_eval), gt = FALSE, lt = TRUE, eq = TRUE)
           }
           if (is.integer(lhs_eval)) {
             if (!is.integer(rhs_eval)) {
               if (as.integer(rhs_eval) != rhs_eval && rhs_eval < 0) {
                 # as.integer truncates *towards* zero so -2.5 => 2
                 # -3 <= -2.5 == 2
                 # yet 2.5 <= 2
                 rhs_eval <- as.integer(rhs_eval) - 1L
               } else {
                 rhs_eval <- as.integer(rhs_eval)
               }
             }
             o <- AnyWhich_int(lhs_eval, rhs_eval, gt = FALSE, lt = TRUE, eq = TRUE)
           }
         },
         "<" = {
           if (is.double(lhs_eval)) {
             o <- AnyWhich_dbl(lhs_eval, as.double(rhs_eval), gt = FALSE, lt = TRUE, eq = FALSE)
           }
           if (is.integer(lhs_eval)) {
             if (!is.integer(rhs_eval)) {
               if (as.integer(rhs_eval) != rhs_eval && rhs_eval > 0) {
                 #  2.5 =>  2L
                 # -2.5 => -2L
                 rhs_eval <- as.integer(rhs_eval) + 1L
               } else {
                 rhs_eval <- as.integer(rhs_eval)
               }
             }
             o <- AnyWhich_int(lhs_eval, rhs_eval, gt = FALSE, lt = TRUE, eq = FALSE)
           }
         },
         ">=" = {
           if (is.double(lhs_eval)) {
             o <- AnyWhich_dbl(lhs_eval, as.double(rhs_eval), gt = TRUE, lt = FALSE, eq = TRUE)
           }
           if (is.integer(lhs_eval)) {
             if (!is.integer(rhs_eval)) {
               if (as.integer(rhs_eval) != rhs_eval && rhs_eval > 0) {
                 #  2.5 =>  2L
                 # -2.5 => -2L
                 rhs_eval <- as.integer(rhs_eval) + 1L
               } else {
                 rhs_eval <- as.integer(rhs_eval)
               }
             }
             o <- AnyWhich_int(lhs_eval, rhs_eval, gt = TRUE, lt = FALSE, eq = TRUE)
           }
         },
         ">" = {
           if (is.double(lhs_eval)) {
             o <- AnyWhich_dbl(lhs_eval, as.double(rhs_eval), gt = TRUE, lt = FALSE, eq = FALSE)
           }
           if (is.integer(lhs_eval)) {
             if (!is.integer(rhs_eval)) {
               if (as.integer(rhs_eval) != rhs_eval && rhs_eval > 0) {
                 #  2.5 =>  2L
                 # -2.5 => -2L
                 rhs_eval <- as.integer(rhs_eval) + 1L
               } else {
                 rhs_eval <- as.integer(rhs_eval)
               }
             }
             o <- AnyWhich_int(lhs_eval, rhs_eval, gt = TRUE, lt = FALSE, eq = FALSE)
           }
         },
         "%in%" = {
           o <-
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
         }
         # nocov end
         )
  return(o)
}



