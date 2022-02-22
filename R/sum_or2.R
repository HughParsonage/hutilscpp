

sum_or2 <- function(exprA, exprB, nThread = getOption("hutilscpp.nThread", 1L)) {
  sexprA <- substitute(exprA)

  oo1 <- xx1 <- yy1 <-
    oo2 <- xx2 <- yy2 <- NULL

  if (length(sexprA) == 3L) {
    oo1 <- as.character(sexprA[[1L]])
    xx1 <- eval.parent(sexprA[[2L]])
    yy1 <- eval.parent(sexprA[[3L]])
  } else if (length(sexprA) == 2L) {
    oo1 <- as.character(sexprA[[1L]])
    xx1 <- eval.parent(sexprA[[2L]])
  } else {
    oo1 <- "=="
    xx1 <- exprA
  }

  if (!is.null(xx1) && length(xx1) <= 1e3L) {
    return(sum(exprA | exprB, na.rm = TRUE))
  }

  if (!missing(exprB)) {
    sexprB <- substitute(exprB)
    if (length(sexprB) == 3L) {
      oo2 <- as.character(sexprB[[1L]])
      xx2 <- eval.parent(sexprB[[2L]])
      yy2 <- eval.parent(sexprB[[3L]])
    } else if (length(sexprB) == 2L) {
      oo2 <- as.character(sexprB[[1L]])
      xx2 <- eval.parent(sexprB[[2L]])
    } else {
      oo2 <- "=="
      xx2 <- exprB
    }
  }

  .Call("Csum_or2",
        oo1, xx1, yy1,
        oo2, xx2, yy2,
        nThread,
        PACKAGE = "hutilscpp")

}
