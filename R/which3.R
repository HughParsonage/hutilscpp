#' which of three vectors are the elements (all, any) true?
#' @param x,y,z Logical vectors. Either the same length or length-1
#' @param And Boolean. If \code{TRUE}, only indices where all of x, y, z
#' are TRUE are returned; if \code{FALSE}, any index where x, y, z
#' are TRUE are returned.
#' @param anyNAx,anyNAy,anyNAz Whether or not the inputs have \code{NA}.
#' @export

which3 <- function(x, y, z,
                   And = TRUE,
                   anyNAx = anyNA(x),
                   anyNAy = anyNA(y),
                   anyNAz = anyNA(z)) {
  stopifnot(is.logical(x), is.logical(y), is.logical(z))
  check_TF(And)
  if (anyNAx || anyNAy || anyNAz) {
    cpp_list <- .Call("Cwhich3", x, y, z, And, anyNAx, anyNAy, anyNAz, PACKAGE = packageName)
    {cpp_list[[2]]}[seq_len(cpp_list[[1]])]
  } else {
    .Call("Cwhich3_mem", x, y, z, And, PACKAGE = packageName)
  }
}

which_and2s <- function(exprA, exprB, ..., .parent_nframes = 1L,
                        Ion = NULL,
                        nThread = getOption("hutilscpp.nThread", 1L)) {
  nThread <- check_omp(nThread)

  sexprA <- substitute(exprA)

  if (missing(exprB) && length(sexprA) == 3L) {
    ans <-
      .Call("C_which_and1s",
            as.character(sexprA[[1L]]),
            eval.parent(sexprA[[2L]], n = .parent_nframes),
            eval.parent(sexprA[[3L]], n = .parent_nframes),
            Ion,
            PACKAGE = "hutilscpp")
    if (is.null(ans)) {
      return(which(exprA))
    } else {
      return(ans)
    }
  }
  sexprB <- substitute(exprB)
  ans <- NULL
  if (length(sexprA) == 3L && length(sexprB) == 3L) {
    ans <- w3(eval.parent(sexprA[[2L]], n = .parent_nframes),
              op2M(as.character(sexprA[[1L]])),
              eval.parent(sexprA[[3L]], n = .parent_nframes),

              eval.parent(sexprB[[2L]], n = .parent_nframes),
              op2M(as.character(sexprB[[1L]])),
              eval.parent(sexprB[[3L]], n = .parent_nframes))
  }
  if (is.null(ans)) {
    return(which(and3s(exprA, exprB, nThread = nThread, .parent_nframes = .parent_nframes + 1L)))
  }
  ans
}

w3 <- function(x1, o1, y1,
               x2, o2, y2,
               Ion = length(x1)) {
  s <-
  .Call("C_which_and2s",
        x1, o1, y1,
        x2, o2, y2,
        getOption("hutilscpp.nThread", 1L),
        Ion,
        PACKAGE = "hutilscpp")
  s
}



