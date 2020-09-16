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
#' @param verbose \describe{
#' \item{\code{logical(1)}, default: \code{FALSE}}{If \code{TRUE} a message is emitted
#' if \code{expr} could not be handled in the advertised way.}
#' }
#'
#' @param reverse \describe{
#'   \item{\code{logical(1)}, default: \code{FALSE}}{Scan \code{expr} in reverse.}
#' }
#'
#' @param sexpr Equivalent to \code{substitute(expr)}. For internal use.
#' @param eval_parent_n Passed to \code{eval.parent}, the environment in which
#' \code{expr} is evaluated.
#'
#' @param suppressWarning Either a \code{FALSE} or \code{TRUE}, whether or not
#' warnings should be suppressed. Also supports a string input which suppresses a
#' warning if it matches as a regular expression.
#'
#' @param use.which.max If \code{TRUE}, \code{which.max} is dispatched immediately,
#' even if \code{expr} would be amenable to separation. Useful when evaluating
#' many small \code{expr}'s when these are known in advance.
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
#' @export which_first


which_first <- function(expr,
                        verbose = FALSE,
                        reverse = FALSE,
                        sexpr,
                        eval_parent_n = 1L,
                        suppressWarning = getOption("hutilscpp_suppressWarning", FALSE),
                        use.which.max = FALSE) {
  if (use.which.max) {
    return(.which_first(expr, verbose = verbose, reverse = reverse))
  }
  rhs <- NULL
  if (missing(sexpr)) {
    sexpr <- substitute(expr)
  }
  if (!is.call(sexpr) ||
      length(sexpr) != 3L ||
      !(op <- do_op2M(operator <- as.character(sexpr[[1L]])))) {
    o <- .which_first(expr, verbose = verbose, reverse = reverse)
    return(o)
  }
  lhs <- sexpr[[2L]]

  if (!is.name(lhs)) {
    o <- .which_first(expr, verbose = verbose, reverse = reverse)
    return(o)
  }

  rhs <- sexpr[[3L]]

  isValidExpr <-
    is.numeric(rhs) ||  # bare doubles and integers
    AND(is.call(rhs) && is.numeric(rhs[[2L]]),
        # negatives
        as.character(rhs[[1L]]) == "-") ||
    is.logical(rhs_eval <- eval.parent(rhs, n = eval_parent_n)) ||

    # c(0, 1, 2) is not numeric but it is when evaluated
    AND(op >= 7 && op <= 10,
        is.numeric(rhs_eval)) ||

    # x != y  or  x == y

    AND(is.symbol(rhs),
        # Must be both integers or both doubles otherwise will fall through too late.
        # Too expensive to check or coerce to doubles in case.
        AND(is.numeric(lhs_eval <- eval.parent(lhs, n = eval_parent_n)),
            is.numeric(rhs_eval))) ||

    AND(is.character(rhs_eval),
        # Must be both integers or both doubles otherwise will fall through too late.
        # Too expensive to check or coerce to doubles in case.
        all(is.character(lhs_eval <- eval.parent(lhs, n = eval_parent_n)),
            OR(length(lhs_eval) == length(rhs_eval),
               length(rhs_eval) == 1L),
            OR(operator == "!=",
               operator == "=="),
            na.rm = TRUE))



  if (!isValidExpr) {
    o <- .which_first(expr, verbose = verbose, reverse = reverse)
    return(o)
  }

  lhs_eval <- eval.parent(lhs, n = eval_parent_n)

  if (length(lhs_eval) <= 1L) {
    if (isTRUE(expr)) {
      return(1L)
    } else {
      return(0L)
    }
  }
  rhs_eval <- eval.parent(rhs, n = eval_parent_n)


  if (is.logical(lhs_eval)) {
    if (operator == "%in%") {
      # miniaturize rhs to be subset of c(TRUE, FALSE, NA)
      rhs_eval_mini <- c(if (anyNA(rhs_eval)) NA,
                         if (any(rhs_eval, na.rm = TRUE)) TRUE,
                         if (!all(rhs_eval, na.rm = TRUE)) FALSE)
      if (length(rhs_eval_mini) == 3L) {
        return(if (reverse) length(lhs_eval) else 1L) # all must be present
      }
      if (length(rhs_eval_mini) == 1L) {
        if (anyNA(rhs_eval_mini)) {
          if (anyNA(lhs_eval)) {
            if (reverse) {
              return(do_which_last(is.na(lhs_eval)))
            } else {
              return(which.max(is.na(lhs_eval)))
            }
          } else {
            return(0L)
          }
        }
        return(.which_first_logical(lhs_eval, rhs_eval_mini, rev = reverse))
      }
      # must be length 2
      if (!anyNA(rhs_eval_mini)) {
        # TRUE or FALSE
        o1 <- .which_first_logical(lhs_eval, TRUE, rev = reverse)
        if (o1 == 0) {
          return(.which_first_logical(lhs_eval, FALSE, rev = reverse))
        }
        o2 <- .which_first_logical(lhs_eval[seq_len(o1)], FALSE, rev = reverse)
        if (o2 == 0) {
          return(o1)
        }
        if (reverse) {
          return(max(o1, o2))
        } else {
          return(min(o1, o2))
        }
      } else {
        # NA and {TRUE or FALSE}
        if (any(rhs_eval_mini, na.rm = TRUE)) {
          # NA and TRUE
          if (reverse) {
            return(do_which_last_notFALSE(lhs_eval))
          }
          o1 <- .which_first_logical(lhs_eval, TRUE, rev = reverse)
        } else {
          # NA and FALSE
          if (reverse) {
            return(do_which_last_notTRUE(lhs_eval))
          } else {
            return(do_which_first_notTRUE(lhs_eval))
          }
        }
        if (o1 == 0L) {
          if (anyNA(lhs_eval)) {
            return(which.max(is.na(lhs_eval)))
          } else {
            return(0L)
          }
        }

        if (anyNA(lhs_eval_1 <- lhs_eval[seq_len(o1)])) {
          return(which.max(is.na(lhs_eval_1)))
        } else {
          return(o1)
        }
      }
      stop("Internal error: which_first_logical:165:20190823. Please report") # nocov
    }

    if (length(rhs_eval) == length(lhs_eval)) {
      if (is.logical(rhs_eval)) {
        # is.logical(rhs_eval)  necessary to ensure integers don't falsely resemble TRUE
        # e.g. which_first(c(TRUE, TRUE) != c(1L, 2L)) should be 2 not 0.
        # Quicker to do this, apparently.
        if (identical(as.logical(lhs_eval),
                      as.logical(rhs_eval))) {
          if (operator == "==" || operator == "<=" || operator == ">=" || operator == "%in%") {
            return(1L)
          } else {
            return(0L)
          }
        } else {
          do_which_first_lgl_lgl_op(lhs_eval, rhs_eval, op, reverse = TRUE)
        }
      }
    }

    if (anyNA(rhs_eval)) {
      warn_msg <- "`rhs` appears to be logical NA. Treating as\n\twhich_first(is.na(.))"
      if (operator == "!=") {
        warn_msg <- "`rhs` appears to be logical NA. Treating as\n\twhich_first(!is.na(.))"
      }
      suppressThisWarn <-
        isTRUE(suppressWarning) ||
        AND(is.character(suppressWarning),
            any(vapply(suppressWarning, grepl, x = warn_msg, perl = TRUE, FUN.VALUE = FALSE),
                na.rm = TRUE))

      switch(operator,
             "==" = {
               if (!suppressThisWarn) {
                 warning(warn_msg)
               }
               o <- .which_first(is.na(lhs_eval), verbose = verbose, reverse = reverse)
             },
             "!=" = {
               if (!suppressThisWarn) {
                 warning(warn_msg)
               }
               o <- .which_first(!is.na(lhs_eval), verbose = verbose, reverse = reverse)
             },
             stop("`rhs` appears to be logical NA. This is not supported for operator '", operator, "'."))
    } else {
      o <- .which_first_logical(lhs_eval, as.logical(rhs_eval), operator = operator, rev = reverse)
    }
    return(o)
  }

  if (op < do_op2M("%in%") && length(lhs_eval) != length(rhs_eval) && length(rhs_eval) != 1L) {
    stop("In `which_first(<lhs> ", operator, " <rhs>)`, the length of <rhs> was neither ",
         "`length(<lhs>) = ", length(lhs_eval), "` nor 1. Such recycling is not supported")
  }

  if (is.numeric(lhs_eval) && is.numeric(rhs_eval) &&
      (length(lhs_eval) == length(rhs_eval) || op)) {
    return(do_which_first_n(lhs_eval, rhs_eval, op, reverse))
  }

  if (is.character(lhs_eval)) {
    oc <-
      switch(operator,
             "==" = AnyCharMatch(lhs_eval, as.character(rhs_eval)),
             "!=" = AnyCharMatch(lhs_eval, as.character(rhs_eval), opposite = TRUE),
             {
               o <- .which_first(expr, verbose = verbose, reverse = reverse)
             })
    if (oc <= .Machine$integer.max) {
      oc <- as.integer(oc)
    }
    return(oc)
  }

  o <- .which_first(expr, reverse = reverse)
  # any which's return R_xlen_t
  if (o <= .Machine$integer.max) {
    o <- as.integer(o)
  }
  o
}

.which_first <- function(expr, verbose = FALSE, reverse = FALSE) {
  if (verbose) {
    message("Falling back to `which.max(expr)`.")
  }
  if (reverse) {
    return(do_which_last(expr))
  }
  o <- which.max(expr)
  if (length(o) == 0L) {
    # i.e. all expr NA
    return(0L)
  }
  # o == 1L is wrong if all expr are FALSE
  if (o == 1L && !expr[1L]) {
    o <- 0L
  }
  o
}

.which_first_logical <- function(lhs, rhs, operator = "==", verbose = FALSE, rev = FALSE) {
  stopifnot(length(lhs) >= 1L,
            is.logical(rhs), length(rhs) == 1L, !anyNA(rhs),
            is.logical(verbose))

  #    operator   rhs   first/last
  # 1:       != FALSE   notFALSE
  # 2:       !=  TRUE   notTRUE
  switch({
    operator
  },
  "!=" = {
    if (rhs) {
      if (rev) {
        return(do_which_last_false(lhs))
      } else {
        return(do_which_first_false(lhs))
      }
    } else {
      if (rev) {
        return(do_which_last(lhs))
      } else {
        return(do_which_first(lhs))
      }
    }
  },
  "==" = {
    # 0:       ==  TRUE   TRUE
    # 9:       == FALSE   FALSE
    if (rhs) {
      if (rev) {
        return(do_which_last(lhs))
      } else {
        return(do_which_first(lhs))
      }
    } else {
      if (rev) {
        return(do_which_last_false(lhs))
      } else {
        return(do_which_first_false(lhs))
      }
    }
  },
  "<" = {
    # 5:        < FALSE   0L
    # 6:        <  TRUE   FALSE
    if (rhs) {
      if (rev) {
        return(do_which_last_false(lhs))
      } else {
        return(do_which_first_false(lhs))
      }
    } else {
      return(0L)
    }
  },
  "<=" = {
    # 8:       <=  TRUE   1L/length
    # 7:       <= FALSE   FALSE
    if (rhs) {
      if (rev) {
        return(length(lhs))
      } else {
        return(1L)
      }
    } else {
      if (rev) {
        return(do_which_last_false(lhs))
      } else {
        return(do_which_first_false(lhs))
      }
    }
  },
  ">" = {
    # 2:        >  TRUE   0L
    # 1:        > FALSE   TRUE
    if (rhs) {
      return(0L)
    } else {
      if (rev) {
        return(do_which_last(lhs))
      } else {
        return(do_which_first(lhs))
      }
    }
  },
  ">=" = {
    # 4:       >=  TRUE   TRUE
    # 3:       >= FALSE   TRUE/FALSE
    if (rhs) {
      if (rev) {
        return(do_which_last(lhs))
      } else {
        return(do_which_first(lhs))
      }
    } else {
      if (rev) {
        return(length(lhs))
      } else {
        return(1L)
      }
    }
  })
  # nocov start
  print(ls.str())
  stop("Internal error: 559:2019:11:18. ")
  # nocov end
}


#' @rdname which_first
#' @export which_last
which_last <- function(expr,
                       verbose = FALSE,
                       reverse = FALSE,
                       suppressWarning = getOption("hutilscpp_suppressWarning", FALSE)) {
  force(reverse)
  which_first(expr,
              verbose = verbose,
              reverse = isFALSE(reverse),
              sexpr = substitute(expr),
              eval_parent_n = 2L,
              suppressWarning = suppressWarning)
}








