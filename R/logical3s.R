#' Complex logical expressions
#' @name logical3s
#' @description Performant implementations of \code{&} et \code{or}.
#' Performance is high when the expressions are long (i.e. over 10M elements)
#' and in particular when they are of the form \code{lhs <op> rhs} for binary
#' \code{<op>}.
#' @param exprA,exprB,exprC,... Expressions of the form \code{x <op> y}.
#' with \code{<op>} one of the standard binary operators.
#'
#' Only \code{exprA} is required, all following expressions are optional.
#'
#' @param .parent_nframes \describe{
#' \item{\code{integer(1)}}{For internal use. Passed to \code{eval.parent}.}
#' }
#' @param nThread \describe{
#' \item{\code{integer(1)}}{Number of threads to use.}
#' }
#'
#' @param type The type of the result. \code{which} corresponds to the
#' indices of \code{TRUE} in the result. Type \code{raw} is available
#' for a memory-constrained result, though the result will not be
#' interpreted as logical.
#'
#'
#'
#'
#'
#' @return
#'
#' \code{and3s} and \code{or3s} return \code{exprA & exprB & exprC} and
#' \code{exprA | exprB | exprC} respectively. If any expression is missing
#' it is considered \code{TRUE} for \code{and3s} and \code{FALSE} for \code{or3s};
#' in other words only the results of the other expressions count towards the result.
#'
#'
#'
NULL

#' @rdname logical3s
#' @export
and3s <- function(exprA, exprB = NULL, exprC = NULL,
                  ...,
                  nThread = getOption("hutilscpp.nThread", 1L),
                  .parent_nframes = 1L,
                  type = c("logical", "raw", "which")) {

  sexprA <- substitute(exprA)
  type <-
    switch(type[[1L]],
           raw = "raw",
           logical = "logical",
           which = "which",
           "raw")

  oo1 <- xx1 <- yy1 <-
    oo2 <- xx2 <- yy2 <- NULL

  if (length(sexprA) == 3L) {
    oo1 <- as.character(sexprA[[1L]])
    xx1 <- eval.parent(sexprA[[2L]], n = .parent_nframes)
    yy1 <- eval.parent(sexprA[[3L]], n = .parent_nframes)
  } else if (length(sexprA) == 2L) {
    oo1 <- as.character(sexprA[[1L]])
    xx1 <- eval.parent(sexprA[[2L]], n = .parent_nframes)
  } else {
    oo1 <- "=="
    xx1 <- exprA
  }
  if (!is.null(xx1) && length(xx1) <= 1e3L) {
    # Don't bother (or still NULL)
    ans <- .et3(exprA, exprB, exprC, ...)
    return(switch(type,
                  raw = lgl2raw(ans, nThread = nThread),
                  logical = ans,
                  which = which(ans)))
  }

  if (!missing(exprB) && !is.null(sexprB <- substitute(exprB))) {
    if (length(sexprB) == 3L) {
      oo2 <- as.character(sexprB[[1L]])
      xx2 <- eval.parent(sexprB[[2L]], n = .parent_nframes)
      yy2 <- eval.parent(sexprB[[3L]], n = .parent_nframes)
    } else if (length(sexprB) == 2L) {
      oo2 <- as.character(sexprB[[1L]])
      xx2 <- eval.parent(sexprB[[2L]], n = .parent_nframes)
    } else {
      oo2 <- "=="
      xx2 <- exprB
    }
  }
  if (!is.raw(xx1)) {
    switch(oo1,
           "%in%" = {
             xx1 <- finp(xx1, yy1, nThread = nThread, .raw = 1L)
             yy1 <- NULL
             oo1 <- "=="
           },
           "%notin%" = {
             xx1 <- fnotinp(xx1, yy1, nThread = nThread, .raw = 1L)
             yy1 <- NULL
             oo1 <- "=="
           })
  }
  if (is.character(oo2) && !is.raw(xx2)) {
    switch(oo2,
           "%in%" = {
             xx2 <- finp(xx2, yy2, nThread = nThread, .raw = 1L)
             yy2 <- NULL
             oo2 <- "=="
           },
           "%notin%" = {
             xx2 <- fnotinp(xx2, yy2, nThread = nThread, .raw = 1L)
             yy2 <- NULL
             oo2 <- "=="
           })
  }

  ans <-
    .Call("Cands",
          oo1, xx1, yy1,
          oo2, xx2, yy2,
          nThread,
          PACKAGE = "hutilscpp")


  if (is.null(ans)) {
    message("Falling back to `&`")
    # fall back
    ans <- return(Reduce("&", list(exprA, exprB %||% TRUE, exprC %||% TRUE, ...)))
    return(switch(type,
                  raw = lgl2raw(ans),
                  logical = ans,
                  which = which(ans)))
  }

  if ((missing(exprC) || is.null(substitute(exprC))) && missing(..1)) {
    return(switch(type,
                  raw = ans,
                  logical = raw2lgl(ans, nThread = nThread),
                  which = which_raw(ans)))
  }
  .and_raw(ans,
           eval.parent(substitute(and3s(exprC, ...,
                                        nThread = nThread,
                                        type = "raw"))),
           nThread = nThread)
  return(switch(type,
                raw = ans,
                logical = raw2lgl(ans, nThread = nThread),
                which = which_raw(ans)))
}

#' @rdname logical3s
#' @export
or3s <- function(exprA, exprB = NULL, exprC = NULL,
                  ...,
                  nThread = getOption("hutilscpp.nThread", 1L),
                  .parent_nframes = 1L,
                  type = c("logical", "raw", "which")) {
  type <-
    switch(type[[1L]],
           raw = "raw",
           logical = "logical",
           which = "which",
           "raw")
  if (missing(exprB) && !missing(exprC)) {
    if (missing(..1)) {
      return(eval.parent(substitute(or3s(exprA, exprC, nThread = nThread, type = type))))
    } else {
      return(eval.parent(substitute(or3s(exprA, exprC, ..., nThread = nThread, type = type)))) # nocov
    }
  }
  sexprA <- substitute(exprA)


  oo1 <- xx1 <- yy1 <-
    oo2 <- xx2 <- yy2 <- NULL

  if (length(sexprA) == 3L) {
    oo1 <- as.character(sexprA[[1L]])
    xx1 <- eval.parent(sexprA[[2L]], n = .parent_nframes)
    yy1 <- eval.parent(sexprA[[3L]], n = .parent_nframes)
  } else if (length(sexprA) == 2L) {
    oo1 <- as.character(sexprA[[1L]])
    xx1 <- eval.parent(sexprA[[2L]], n = .parent_nframes)
  } else {
    oo1 <- "=="
    xx1 <- exprA
  }
  if (!is.null(xx1) && length(xx1) <= 1e3L) {
    # Don't bother (or still NULL)
    ans <- .or3(exprA, exprB, exprC, ...)
    return(switch(type,
                  raw = lgl2raw(ans),
                  logical = ans,
                  which = which(ans)))
  }

  if (!missing(exprB) && !is.null(sexprB <- substitute(exprB))) {
    if (length(sexprB) == 3L) {
      oo2 <- as.character(sexprB[[1L]])
      xx2 <- eval.parent(sexprB[[2L]], n = .parent_nframes)
      yy2 <- eval.parent(sexprB[[3L]], n = .parent_nframes)
    } else if (length(sexprB) == 2L) {
      oo2 <- as.character(sexprB[[1L]])
      xx2 <- eval.parent(sexprB[[2L]], n = .parent_nframes)
    } else {
      oo2 <- "=="
      xx2 <- exprB
    }
  }

  switch(oo1,
         "%in%" = {
           xx1 <- finp(xx1, yy1, nThread = nThread)
           yy1 <- NULL
           oo1 <- "=="
         },
         "%notin%" = {
           xx1 <- fnotinp(xx1, yy1, nThread = nThread)
           yy1 <- NULL
           oo1 <- "=="
         })

  if (is.character(oo2)) {
    switch(oo2,
           "%in%" = {
             xx2 <- finp(xx2, yy2, nThread = nThread)
             yy2 <- NULL
             oo2 <- "=="
           },
           "%notin%" = {
             xx2 <- fnotinp(xx2, yy2, nThread = nThread)
             yy2 <- NULL
             oo2 <- "=="
           })
  }

  ans <-
    .Call("Cors",
          oo1, xx1, yy1,
          oo2, xx2, yy2,
          nThread,
          PACKAGE = "hutilscpp")
  # nocov start
  if (is.null(ans)) {
    message("Falling back to `|`")
    # fall back
    return(Reduce("|", list(exprA, exprB %||% FALSE, exprC %||% FALSE, ...)))
  }
  # nocov end

  if (missing(exprC) && missing(..1)) {
    return(switch(type,
                  raw = ans,
                  logical = raw2lgl(ans, nThread = nThread),
                  which = which_raw(ans)))
  }
  .or_raw(ans,
          eval.parent(substitute(or3s(exprC, ...,
                                      nThread = nThread,
                                      type = "raw"))),
          nThread = nThread)
  return(switch(type,
                raw = ans,
                logical = raw2lgl(ans, nThread = nThread),
                which = which_raw(ans)))
}



do_par_in <- function(x, tbl, nThread = 1L) {
  nThread <- check_omp(nThread)
  if (is.integer(x) && is.integer(tbl)) {
    .Call("Cpar_in_int", x, tbl, nThread, PACKAGE = packageName)
  } else {
    x %in% tbl # nocov
  }
}

.or3 <- function(x, y, ...) {
  if ((missing(x) || is.null(x)) && (missing(y) || is.null(y))) {
    if (missing(..1)) {
      return(FALSE)
    } else {
      return(.or3(...))
    }
  }
  if (missing(y) || is.null(y)) {
    return(x)
  }
  if (missing(..1)) {
    return(x | y)
  }
  x | y| .or3(...)
}
.et3 <- function(x, y, ...) {
  if ((missing(x) || is.null(x)) && (missing(y) || is.null(y))) {
    if (missing(..1)) {
      return(TRUE)
    } else {
      return(.et3(...)) # nocov
    }
  }
  if (missing(y) || is.null(y)) {
    return(x)
  }
  if (missing(..1)) {
    return(x & y)
  }
  x & y & .et3(...)
}






