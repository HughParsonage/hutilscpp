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
#' interpreted as logical. Because raw masks cannot represent missing
#' values, \code{type = "raw"} is not allowed with \code{na = "base"}.
#'
#' @param na \describe{
#' \item{\code{"C"} (default)}{Historical high-performance behaviour:
#' missing values are interpreted by the C kernels without extra R-level
#' NA handling. This preserves the package's CRAN behaviour, including
#' C-level treatment of \code{NA_INTEGER}, \code{NA_LOGICAL}, and
#' \code{NaN}.}
#' \item{\code{"false"}}{Two-valued mask: when a parsed predicate value
#' contains \code{NA} or \code{NaN}, the wrapper evaluates the affected
#' expression chain with base R and coerces resulting \code{NA} values
#' to \code{FALSE}.}
#' \item{\code{"base"}}{Preserve base R three-valued semantics:
#' \code{NA} propagates exactly as in \code{&} / \code{|}, including
#' across three or more predicates (the recursive call returns
#' \code{logical} when \code{na = "base"} so the AND / OR combination
#' in R preserves \code{NA}).  When any parsed predicate value
#' contains \code{NA} the wrapper defers to \code{Reduce("&", \dots)}
#' / \code{Reduce("|", \dots)} for the first two predicates as well.}
#' }
#'
#' @param unsupported \describe{
#' \item{\code{"fallback"} (default)}{When the C kernel cannot handle a
#' type/op/length combination, silently fall back to base R
#' \code{Reduce("&", \dots)} / \code{Reduce("|", \dots)}. Matches the
#' package's historical behaviour.}
#' \item{\code{"error"}}{Raise an error instead. Useful in tests so
#' silent unsupported paths fail loudly in CI.}
#' }
#'
#' @param recycle \describe{
#' \item{\code{"base"} (default)}{Match base R's recycling rules: when a
#' predicate's RHS length is not in \code{\{1, length(LHS), 2 for
#' between\}}, the kernel reports an unsupported type/length, and the
#' \code{unsupported} setting decides what happens next.}
#' \item{\code{"strict"}}{Reject any RHS length not in
#' \code{\{1, length(LHS), 2 for between\}} with an error before the
#' kernel is even called.}
#' }
#'
#' @return
#'
#' \code{and3s} and \code{or3s} return \code{exprA & exprB & exprC} and
#' \code{exprA | exprB | exprC} respectively. If any expression is missing
#' it is considered \code{TRUE} for \code{and3s} and \code{FALSE} for \code{or3s};
#' in other words only the results of the other expressions count towards the result.
#'
#' @section Note on NA / NaN:
#'
#' The default \code{na = "C"} path preserves the historical kernel
#' semantics. This is intentionally low-level: for example, a bare
#' logical \code{NA} is passed to C as \code{NA_LOGICAL} and is truthy
#' in the unary-mask path, while comparisons against \code{NaN} follow
#' the relevant C comparison or special-case kernel branch. Use
#' \code{na = "false"} for an explicit two-valued filter mask where
#' missing predicate results are \code{FALSE}, or \code{na = "base"} to
#' propagate \code{NA} like base R.
#'
#' @section Performance:
#'
#' The wrapper carries a fixed R-level cost per call (substitute,
#' \code{eval.parent}, option validation, \code{\dots}-forwarding) on
#' the order of \emph{tens of microseconds} for \code{and3s} /
#' \code{or3s} and roughly twice that for \code{sum_and3s} /
#' \code{sum_or3s}. The C kernel only runs when \code{length(LHS) > 1000};
#' shorter inputs take a base-R shortcut where the wrapper cost
#' dominates.
#'
#' Rule of thumb: \code{and3s} pays for itself when the per-call work
#' it saves exceeds the wrapper overhead. In practice that means
#' single calls on vectors of \emph{at least a few thousand elements}
#' for \code{and3s} (more for \code{sum_and3s}). On a single 1e9-row
#' vector the kernel path is comfortably faster than base \code{&};
#' on a vector of 100 it is several times slower.
#'
#' This makes the family a poor fit for the \code{j=} expression of a
#' \code{data.table} \code{by=} / \code{keyby=} call when the grouping
#' has high cardinality and small groups: every group calls the
#' wrapper afresh, and on groups of a few rows you can spend
#' minutes paying R-level overhead that base \code{sum(. & .)} would
#' do in a couple of seconds. Use \code{and3s} / \code{or3s} for
#' single bulk calls; for grouped reductions on small groups, prefer
#' base R inside \code{j=}.
#'
NULL

#' @rdname logical3s
#' @export
and3s <- function(exprA, exprB = NULL, exprC = NULL,
                  ...,
                  na          = c("C", "false", "base"),
                  unsupported = c("fallback", "error"),
                  recycle     = c("base", "strict"),
                  nThread = getOption("hutilscpp.nThread", 1L),
                  .parent_nframes = 1L,
                  type = c("logical", "raw", "which")) {

  na          <- match.arg(na)
  unsupported <- match.arg(unsupported)
  recycle     <- match.arg(recycle)

  sexprA <- substitute(exprA)
  type <-
    switch(type[[1L]],
           raw = "raw",
           logical = "logical",
           which = "which",
           "raw")
  if (na == "base" && type == "raw") {
    stop("`type = \"raw\"` cannot represent `NA` when `na = \"base\"`.",
         call. = FALSE)
  }

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

  # Small-vector shortcut: defer to base R `&` for short inputs when no
  # explicit validation mode is requested. `na = "C"` keeps historical
  # CRAN behaviour; `na = "false"` coerces the base result to a
  # two-valued mask.
  if (na %in% c("C", "false") &&
      recycle == "base" && unsupported == "fallback" &&
      !is.null(xx1) && length(xx1) <= 1e3L) {
    ans <- .et3(exprA, exprB, exprC, ...)
    if (na == "false") ans <- .na_false_logical3s(ans)
    return(switch(type,
                  raw = lgl2raw(ans, nThread = nThread),
                  logical = ans,
                  which = which(ans)))
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

  # Strict recycle validation runs BEFORE the NA-base fallback so that
  # combining `recycle = "strict"` with `na = "base"` doesn't let an
  # NA-containing predicate with mismatched RHS sneak past the strict
  # check. (The whole point of `strict` is "fail loudly on bad shapes".)
  if (recycle == "strict") {
    N <- length(xx1)
    .validate_predicate_length(oo1, xx1, yy1, N, "and3s")
    if (!is.null(oo2)) {
      .validate_predicate_length(oo2, xx2, yy2, N, "and3s")
    }
  }

  # `na = "false"` / `"base"` need R-level missing-value semantics when
  # parsed predicate inputs contain NA / NaN. Validate explicit error
  # modes before the fallback so missing values cannot mask unsupported
  # or strictly invalid predicates.
  if (na %in% c("false", "base") &&
      .predicate_has_na_logical3s(xx1, yy1, xx2, yy2)) {
    if (unsupported == "error") {
      probe <-
        .Call("Cands",
              oo1, xx1, yy1,
              oo2, xx2, yy2,
              nThread,
              PACKAGE = "hutilscpp")
      if (is.null(probe)) {
        .stop_unsupported_logical3s("and3s", "&")
      }
    }
    if ((unsupported == "error" || recycle == "strict") &&
        (!(missing(exprC) || is.null(substitute(exprC))) || !missing(..1))) {
      suppressMessages(eval.parent(substitute(and3s(exprC, ...,
                                                     na = "false",
                                                     unsupported = unsupported,
                                                     recycle = recycle,
                                                     nThread = nThread,
                                                     type = "raw"))))
    }
    args <- list(exprA, exprB %||% TRUE, exprC %||% TRUE, ...)
    args <- lapply(args, function(a) if (is.raw(a)) raw2lgl(a, nThread = nThread) else a)
    ans <- Reduce("&", args)
    if (na == "false") ans <- .na_false_logical3s(ans)
    return(switch(type,
                  raw = lgl2raw(ans, nThread = nThread),
                  logical = ans,
                  which = which(ans)))
  }

  ans <-
    .Call("Cands",
          oo1, xx1, yy1,
          oo2, xx2, yy2,
          nThread,
          PACKAGE = "hutilscpp")


  if (is.null(ans)) {
    if (unsupported == "error") {
      .stop_unsupported_logical3s("and3s", "&")
    }
    args <- list(exprA, exprB %||% TRUE, exprC %||% TRUE, ...)
    args <- lapply(args, function(a) if (is.raw(a)) raw2lgl(a, nThread = nThread) else a)
    ans <- Reduce("&", args)
    if (na == "false") ans <- .na_false_logical3s(ans)
    return(switch(type,
                  raw = lgl2raw(ans, nThread = nThread),
                  logical = ans,
                  which = which(ans)))
  }

  if ((missing(exprC) || is.null(substitute(exprC))) && missing(..1)) {
    return(switch(type,
                  raw = ans,
                  logical = raw2lgl(ans, nThread = nThread),
                  which = which_raw(ans)))
  }
  if (na == "base") {
    # Preserve base R NA semantics across exprC and further `...`
    # predicates: ask the recursive call for type = "logical" (so NA
    # survives) and combine in R-space with `&` (NA-preserving).
    # `.and_raw` would otherwise force lgl2raw on the recursive result,
    # silently dropping NA -> FALSE.
    rest <- eval.parent(substitute(and3s(exprC, ...,
                                         na = na, unsupported = unsupported, recycle = recycle,
                                         nThread = nThread,
                                         type = "logical")))
    ans_lgl <- raw2lgl(ans, nThread = nThread) & rest
    return(switch(type,
                  raw = lgl2raw(ans_lgl, nThread = nThread),
                  logical = ans_lgl,
                  which = which(ans_lgl)))
  }
  ans <- .and_raw(ans,
                  eval.parent(substitute(and3s(exprC, ...,
                                               na = na, unsupported = unsupported, recycle = recycle,
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
                  na          = c("C", "false", "base"),
                  unsupported = c("fallback", "error"),
                  recycle     = c("base", "strict"),
                  nThread = getOption("hutilscpp.nThread", 1L),
                  .parent_nframes = 1L,
                  type = c("logical", "raw", "which")) {
  na          <- match.arg(na)
  unsupported <- match.arg(unsupported)
  recycle     <- match.arg(recycle)
  type <-
    switch(type[[1L]],
           raw = "raw",
           logical = "logical",
           which = "which",
           "raw")
  if (na == "base" && type == "raw") {
    stop("`type = \"raw\"` cannot represent `NA` when `na = \"base\"`.",
         call. = FALSE)
  }
  if (missing(exprB) && !missing(exprC)) {
    if (missing(..1)) {
      return(eval.parent(substitute(or3s(exprA, exprC,
                                         na = na, unsupported = unsupported, recycle = recycle,
                                         nThread = nThread, type = type))))
    } else {
      return(eval.parent(substitute(or3s(exprA, exprC, ...,
                                         na = na, unsupported = unsupported, recycle = recycle,
                                         nThread = nThread, type = type)))) # nocov
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

  # Small-vector shortcut: see and3s for rationale.
  if (na %in% c("C", "false") &&
      recycle == "base" && unsupported == "fallback" &&
      !is.null(xx1) && length(xx1) <= 1e3L) {
    ans <- .or3(exprA, exprB, exprC, ...)
    if (na == "false") ans <- .na_false_logical3s(ans)
    return(switch(type,
                  raw = lgl2raw(ans, nThread = nThread),
                  logical = ans,
                  which = which(ans)))
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

  # Strict-recycle validation runs BEFORE the NA-base fallback so that
  # combining `recycle = "strict"` with `na = "base"` doesn't let an
  # NA-containing predicate with mismatched RHS bypass the strict check.
  if (recycle == "strict") {
    N <- length(xx1)
    .validate_predicate_length(oo1, xx1, yy1, N, "or3s")
    if (!is.null(oo2)) {
      .validate_predicate_length(oo2, xx2, yy2, N, "or3s")
    }
  }

  # Phase 4: see and3s for rationale.
  if (na %in% c("false", "base") &&
      .predicate_has_na_logical3s(xx1, yy1, xx2, yy2)) {
    if (unsupported == "error") {
      probe <-
        .Call("Cors",
              oo1, xx1, yy1,
              oo2, xx2, yy2,
              nThread,
              PACKAGE = "hutilscpp")
      if (is.null(probe)) {
        .stop_unsupported_logical3s("or3s", "|")
      }
    }
    if ((unsupported == "error" || recycle == "strict") &&
        (!(missing(exprC) || is.null(substitute(exprC))) || !missing(..1))) {
      suppressMessages(eval.parent(substitute(or3s(exprC, ...,
                                                   na = "false",
                                                   unsupported = unsupported,
                                                   recycle = recycle,
                                                   nThread = nThread,
                                                   type = "raw"))))
    }
    args <- list(exprA, exprB %||% FALSE, exprC %||% FALSE, ...)
    args <- lapply(args, function(a) if (is.raw(a)) raw2lgl(a, nThread = nThread) else a)
    ans <- Reduce("|", args)
    if (na == "false") ans <- .na_false_logical3s(ans)
    return(switch(type,
                  raw = lgl2raw(ans, nThread = nThread),
                  logical = ans,
                  which = which(ans)))
  }

  ans <-
    .Call("Cors",
          oo1, xx1, yy1,
          oo2, xx2, yy2,
          nThread,
          PACKAGE = "hutilscpp")
  if (is.null(ans)) {
    if (unsupported == "error") {
      .stop_unsupported_logical3s("or3s", "|")
    }
    args <- list(exprA, exprB %||% FALSE, exprC %||% FALSE, ...)
    args <- lapply(args, function(a) if (is.raw(a)) raw2lgl(a, nThread = nThread) else a)
    ans <- Reduce("|", args)
    if (na == "false") ans <- .na_false_logical3s(ans)
    return(switch(type,
                  raw = lgl2raw(ans, nThread = nThread),
                  logical = ans,
                  which = which(ans)))
  }

  if (missing(exprC) && missing(..1)) {
    return(switch(type,
                  raw = ans,
                  logical = raw2lgl(ans, nThread = nThread),
                  which = which_raw(ans)))
  }
  if (na == "base") {
    # Symmetric to and3s: combine in R-space with `|` so NA from
    # exprC / further `...` predicates propagates instead of being
    # dropped via lgl2raw in `.or_raw`.
    rest <- eval.parent(substitute(or3s(exprC, ...,
                                        na = na, unsupported = unsupported, recycle = recycle,
                                        nThread = nThread,
                                        type = "logical")))
    ans_lgl <- raw2lgl(ans, nThread = nThread) | rest
    return(switch(type,
                  raw = lgl2raw(ans_lgl, nThread = nThread),
                  logical = ans_lgl,
                  which = which(ans_lgl)))
  }
  ans <- .or_raw(ans,
                 eval.parent(substitute(or3s(exprC, ...,
                                             na = na, unsupported = unsupported, recycle = recycle,
                                             nThread = nThread,
                                             type = "raw"))),
                 nThread = nThread)
  return(switch(type,
                raw = ans,
                logical = raw2lgl(ans, nThread = nThread),
                which = which_raw(ans)))
}



# Phase 4: pre-dispatch length validation. Centralised so both and3s and
# or3s share the same `recycle = "strict"` semantics. Returns invisibly
# on success; stops with a descriptive message otherwise. Call only
# after %in% / %notin% preprocessing (those convert yy to NULL).
.validate_predicate_length <- function(oo, xx, yy, n, fname) {
  if (is.null(yy)) return(invisible())
  m <- length(yy)
  if (m == 1L || m == n) return(invisible())
  is_between <- oo %in% c("%between%", "%(between)%", "%]between[%")
  if (is_between && m == 2L) return(invisible())
  stop(sprintf(
    "%s(): `recycle = \"strict\"` and a predicate has RHS length %d, ",
    fname, m),
    sprintf("incompatible with N=%d (allowed: 1, %d%s)",
            n, n, if (is_between) " or 2 for between" else ""),
    call. = FALSE)
}

.stop_unsupported_logical3s <- function(fname, op) {
  stop("`", fname, "()` received an unsupported type/op/length combination ",
       "and `unsupported = \"error\"` was set. Re-run with the default ",
       "(`unsupported = \"fallback\"`) to use base R `", op, "` instead.",
       call. = FALSE)
}

.predicate_has_na_logical3s <- function(xx1, yy1, xx2, yy2) {
  anyNA(xx1) ||
    (!is.null(yy1) && anyNA(yy1)) ||
    (!is.null(xx2) && anyNA(xx2)) ||
    (!is.null(yy2) && anyNA(yy2))
}

.na_false_logical3s <- function(x) {
  if (is.logical(x) && anyNA(x)) return(fcoalesce(x, FALSE))
  x
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
