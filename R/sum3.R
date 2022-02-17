#' Sum of logical expressions
#' @name sum_and3s
#' @aliases sum_or3s
#' @param exprA,exprB,exprC,... Expressions of the form \code{x <op> y}.
#' with \code{<op>} one of the standard binary operators.
#' @param nThread \describe{
#' \item{\code{integer(1)}}{Number of threads to use.}
#' }
#' @param .env The environment in which the expressions are to be evaluated.
#'
#' @return
#' Equivalent to \code{sum(exprA & exprB & exprC)} or
#' \code{sum(exprA | exprB | exprC)} as desired.
#'
#' @export sum_and3s


sum_and3s <- function(exprA, exprB, exprC, ...,
                      nThread = getOption("hutilscpp.nThread", 1L),
                      .env = parent.frame()) {
  sexprA <- substitute(exprA)
  sexprB <- substitute(exprB)
  sexprC <- substitute(exprC)
  missingA <- missing(exprA)
  missingB <- missing(exprB)
  missingC <- missing(exprC)
  oo1 <- xx1 <- yy1 <-
    oo2 <- xx2 <- yy2 <- NULL

  if (length(sexprA) == 3L) {
    oo1 <- as.character(sexprA[[1L]])
    xx1 <- eval(sexprA[[2L]], envir = .env)
    yy1 <- eval(sexprA[[3L]], envir = .env)
  } else if (length(sexprA) == 2L) {
    oo1 <- as.character(sexprA[[1L]])
    xx1 <- eval(sexprA[[2L]], envir = .env)
  } else {
    oo1 <- "=="
    xx1 <- exprA
  }
  if (!is.null(xx1) && length(xx1) <= 1e3L) {
    # Don't bother (or still NULL)
    if (missing(exprC)) {
      if (missing(exprB)) {
        return(sum(exprA, na.rm = TRUE))
      }
      return(sum(exprA & exprB, na.rm = TRUE))
    } else {
      if (missing(exprB)) {
        return(sum(exprA & Reduce("&", list(exprC, ...)), na.rm = TRUE))
      } else {
        if (missing(exprC)) {
          return(sum(exprA & exprB & Reduce("&", list(...)), na.rm = TRUE))
        } else {
          return(sum(exprA & exprB & Reduce("&", list(exprC, ...)), na.rm = TRUE))
        }
      }
    }
  }

  if (!missing(exprB)) {
    sexprB <- substitute(exprB)
    if (length(sexprB) == 3L) {
      oo2 <- as.character(sexprB[[1L]])
      xx2 <- eval(sexprB[[2L]], envir = .env)
      yy2 <- eval(sexprB[[3L]], envir = .env)
    } else if (length(sexprB) == 2L) {
      oo2 <- as.character(sexprB[[1L]])
      xx2 <- eval(sexprB[[2L]], envir = .env)
    } else {
      oo2 <- "=="
      xx2 <- exprB
    }
  }
  ans <-
    .Call("Cands",
          oo1, xx1, yy1,
          oo2, xx2, yy2,
          nThread,
          PACKAGE = "hutilscpp")
  if (missing(exprB) || missing(exprC)) {
    return(sum_raw(ans, nThread = nThread))
  }
  .and_raw(ans,
           eval.parent(and3s(exprC, ...,
                             nThread = nThread,
                             type = "raw")),
           nThread = nThread)

  sum_raw(ans, nThread = nThread)
}

#' @rdname sum_and3s
#' @export

sum_or3s <- function(exprA, exprB, exprC, ...,
                     .env = parent.frame(),
                     nThread = getOption("hutilscpp.nThread", 1L)) {
  sexprA <- substitute(exprA)
  sexprB <- substitute(exprB)
  sexprC <- substitute(exprC)
  missingA <- missing(exprA)
  missingB <- missing(exprB)
  missingC <- missing(exprC)
  oo1 <- xx1 <- yy1 <-
    oo2 <- xx2 <- yy2 <- NULL

  if (length(sexprA) == 3L) {
    oo1 <- as.character(sexprA[[1L]])
    xx1 <- eval(sexprA[[2L]], envir = .env)
    yy1 <- eval(sexprA[[3L]], envir = .env)
  } else if (length(sexprA) == 2L) {
    oo1 <- as.character(sexprA[[1L]])
    xx1 <- eval(sexprA[[2L]], envir = .env)
  } else {
    oo1 <- "=="
    xx1 <- exprA
  }
  if (!is.null(xx1) && length(xx1) <= 1e3L) {
    # Don't bother (or still NULL)
    if (missing(exprC)) {
      if (missing(exprB)) {
        return(sum(exprA, na.rm = TRUE))
      }
      return(sum(exprA | exprB, na.rm = TRUE))
    } else {
      if (missing(exprB)) {
        return(sum(exprA | Reduce("|", list(exprC, ...)), na.rm = TRUE))
      } else {
        if (missing(exprC)) {
          return(sum(exprA | exprB | Reduce("|", list(...)), na.rm = TRUE))
        } else {
          return(sum(exprA | exprB | Reduce("|", list(exprC, ...)), na.rm = TRUE))
        }
      }
    }
  }

  if (!missing(exprB)) {
    sexprB <- substitute(exprB)
    if (length(sexprB) == 3L) {
      oo2 <- as.character(sexprB[[1L]])
      xx2 <- eval(sexprB[[2L]], envir = .env)
      yy2 <- eval(sexprB[[3L]], envir = .env)
    } else if (length(sexprB) == 2L) {
      oo2 <- as.character(sexprB[[1L]])
      xx2 <- eval(sexprB[[2L]], envir = .env)
    } else {
      oo2 <- "=="
      xx2 <- exprB
    }
  }
  ans <-
    .Call("Cors",
          oo1, xx1, yy1,
          oo2, xx2, yy2,
          nThread,
          PACKAGE = "hutilscpp")
  if (missing(exprB) || missing(exprC)) {
    return(sum_raw(ans, nThread = nThread))
  }
  .or_raw(ans,
          eval.parent(or3s(exprC, ...,
                           nThread = nThread,
                           type = "raw")),
          nThread = nThread)

  sum_raw(ans, nThread = nThread)
}

