

ands <- function(exprA, exprB,
                 ...,
                 nThread = getOption("hutilscpp.nThread", 1L),
                 .parent_nframes = 1L,
                 result = c("raw", "logical", "which")) {
  sexprA <- substitute(exprA)
  switch(result,
         raw = NULL,
         logical = NULL,
         which = NULL,
         result <- "raw")


  oo1 <- xx1 <- yy1 <-
    oo2 <- xx2 <- yy2 <- NULL

  if (length(sexprA) == 3L) {
    oo1 <- as.character(sexprA[[1L]])
    xx1 <- eval.parent(sexprA[[2L]], n = .parent_nframes)
    yy1 <- eval.parent(sexprA[[3L]], n = .parent_nframes)
  } else if (length(sexprA) == 2L) {
    oo1 <- as.character(sexprA[[1L]])
    xx1 <- eval.parent(sexprA[[2L]], n = .parent_nframes)
  }
  if (length(xx1) <= 10e3L) {
    # Don't bother (or still NULL)
    if (missing(exprB)) {
      return(exprA)
    }
    return(exprA & exprB)
  }

  if (!missing(exprB)) {
    sexprB <- substitute(exprB)
    if (length(sexprB) == 3L) {
      oo2 <- as.character(sexprB[[1L]])
      xx2 <- eval.parent(sexprB[[2L]], n = .parent_nframes)
      yy2 <- eval.parent(sexprB[[3L]], n = .parent_nframes)
    } else if (length(sexprB) == 2L) {
      oo2 <- as.character(sexprB[[1L]])
      xx2 <- eval.parent(sexprB[[2L]], n = .parent_nframes)
    }
  }

  ans <-
    .Call("Cands",
          oo1, xx1, yy1,
          oo2, xx2, yy2,
          nThread,
          PACKAGE = "hutilscpp")
  if (missing(..1)) {
    return(switch(result,
                  raw = ans,
                  logical = raw2lgl(ans, nThread = nThread),
                  which = which_raw(ans)))
  }
  .and_raw(ans,
           ands(..., .parent_nframes = .parent_nframes + 1L, nThread = nThread, result = "raw"),
           nThread = nThread)
  return(switch(result,
                raw = ans,
                logical = raw2lgl(ans, nThread = nThread),
                which = which_raw(ans)))
}

.and_raw <- function(x, y, z = NULL, nThread = getOption("hutilscpp.nThread", 1L)) {
  .Call("C_and_raw", x, y, z, nThread,  PACKAGE = "hutilscpp")
}




