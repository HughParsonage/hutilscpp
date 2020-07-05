
decompose_expr <- function(sexprA,
                           sexprB,
                           sexprC,
                           .parent_nframes = 1L) {
  eval.parent(substitute({
  X3 <- Y3 <- Z3 <- integer(0)
  A <- B <- C <- logical(0)
  x <- y <- z <- integer(0)
  ox <- oy <- oz <- -1L
  x1 <- y1 <- z1 <- 0L
  x2 <- y2 <- z2 <- 0L

  is_seq <- function(A) {
    identical(A, seq.int(A[1L], along.with = A))
  }

  isBinaryA <- is_binary_sexp(substitute(sexprA), .parent_nframes = .parent_nframes + 1L)
  if (isBinaryA) {
    x <- eval.parent(sexprA[[2]], n = .parent_nframes)
    ox <- attr(isBinaryA, "M")
    rhs_eval <- attr(isBinaryA, "rhs_eval")
    if (ox == op2M("%between%")) { # between so two elements
      x1 <- rhs_eval[[1]]
      x2 <- rhs_eval[[2]]
    } else if (ox == op2M("%in%")) {
      X3 <- rhs_eval

      x1 <- X3[1]
      if (is_seq(X3)) {
        x2 <- X3[length(X3)]
        ox <- op2M("%between%")
      } else {
        if (length(X3) > 100L) {
          A <- x %in% X3
        } else {
          A <- do_par_in(x, X3, nThread = nThread)
        }
        x <- integer(0)
        ox <- -1L
        x2 <- X3[2]
      }
    } else {
      x1 <- x2 <- attr(isBinaryA, "rhs_eval")
    }

  } else if (is_lgl_negation(sexprA)) {
    A <- eval.parent(sexprA[[2]], n = .parent_nframes)
    ox <- 1L
  } else {
    A <- eval.parent(sexprA, n = .parent_nframes)
  }
  if (!missing(sexprB)) {
    isBinaryB <- is_binary_sexp(sexprB, .parent_nframes = .parent_nframes + 1L)
    if (isBinaryB) {
      y <- eval.parent(sexprB[[2]], n = .parent_nframes)
      oy <- attr(isBinaryB, "M")
      rhs_eval <- attr(isBinaryB, "rhs_eval")
      if (oy == op2M("%between%")) { # between so two elements
        y1 <- rhs_eval[[1]]
        y2 <- rhs_eval[[2]]
      } else if (oy == op2M("%in%")) {
        Y3 <- rhs_eval
        stopifnot(length(Y3) > 1)
        y1 <- Y3[1L]
        if (is_seq(Y3)) {
          # Treat x %in% a:b as x %between% c(a, b)
          y2 <- Y3[length(Y3)]
          oy <- op2M("%between%")
        } else {
          if (length(Y3) > 100L) {
            B <- y %in% Y3
          } else {
            B <- do_par_in(y, Y3, nThread = nThread)
          }
          y <- integer(0)
          oy <- -1L
          y2 <- Y3[2]
        }
      } else {
        y1 <- y2 <- rhs_eval
      }

    } else if (is_lgl_negation(sexprB)) {
      B <- eval.parent(sexprB[[2]], n = .parent_nframes)
      oy <- 1L
    } else {
      B <- eval.parent(sexprB, n = .parent_nframes)
    }
  }


  if (!missing(sexprC)) {
    isBinaryC <- is_binary_sexp(sexprC, .parent_nframes = .parent_nframes + 1L)

    if (isBinaryC) {
      z <- eval.parent(sexprC[[2]], n = .parent_nframes)
      oz <- attr(isBinaryC, "M")
      rhs_eval <- attr(isBinaryC, "rhs_eval")
      if (oz == op2M("%between%")) { # between so two elements
        z1 <- rhs_eval[[1]]
        z2 <- rhs_eval[[2]]
      } else if (oz == op2M("%in%")) {
        Z3 <- rhs_eval
        stopifnot(length(Z3) > 1)
        z1 <- Z3[1]
        if (is_seq(Z3)) {
          z2 <- Z3[length(Z3)]
          oz <- op2M("%between%")
        } else {
          if (length(Z3) > 100L) {
            C <- z %in% Z3
          } else {
            C <- do_par_in(z, Z3, nThread = nThread)
          }
          z <- integer(0L)
          oz <- -1L
        }
      } else {
        z1 <- z2 <- rhs_eval
      }

    } else if (is_lgl_negation(sexprC)) {
      C <- eval.parent(sexprC[[2]], n = .parent_nframes)
      oz <- 1L
    } else {
      C <- eval.parent(sexprC, n = .parent_nframes)
    }
  }
  list(x, ox, x1, x2, y, oy, y1, y2, z, oz, z1, z2, A, B, C)
  }))
}






