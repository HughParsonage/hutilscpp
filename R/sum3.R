


sum_and3s <- function(exprA, exprB, exprC, ...,
                      .parent_nframes = 1L,
                      nThread = getOption("hutilscpp.nThread", 1L)) {


  d <-
    eval.parent(substitute({
      sexprA <- substitute(exprA)
      sexprB <- substitute(exprB)
      sexprC <- substitute(exprC)
      decompose_expr(sexprA, sexprB, sexprC, .parent_nframes = .parent_nframes)
    }))

  do_sum3s_par(
    d[[1]],
    d[[2]],
    d[[3]],
    d[[4]],
    d[[5]],
    d[[6]],
    d[[7]],
    d[[8]],
    d[[9]],
    d[[10]],
    d[[11]],
    d[[12]],
    d[[13]],
    d[[14]],
    d[[15]],
    nThread
  )
}

