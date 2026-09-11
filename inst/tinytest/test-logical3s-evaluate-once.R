# Operand extraction must not repeat side effects when a fallback evaluates
# the comparison. Exercise every fallback, both sides, and result formats.
local({
  for (fun in c("and3s", "or3s")) {
    identity <- fun == "and3s"
    for (path in c("small", "scalar", "unsupported", "na")) {
      for (na in c("C", "false", "base")) {
        if (path == "na" && na == "C") next
        for (type in c("logical", "raw", "which")) {
          if (na == "base" && type == "raw") next
          n <- if (path == "small") 6L else 1002L
          calls <- integer(3L)
          lhs <- function() {
            calls[1L] <<- calls[1L] + 1L
            x <- rep.int(calls[1L], n)
            if (path == "na") x[2L] <- NA_integer_
            x
          }
          rhs <- function() {
            calls[2L] <<- calls[2L] + 1L
            rep.int(calls[2L], if (path == "unsupported") 3L else 1L)
          }
          other <- function() {
            calls[3L] <<- calls[3L] + 1L
            rep.int(identity, if (path == "scalar") 1L else n)
          }
          actual <- eval(substitute(FUN(lhs() == rhs(), other(), na = NA_MODE,
                                        type = RESULT_TYPE),
                                    list(FUN = as.name(fun), NA_MODE = na,
                                         RESULT_TYPE = type)))
          expected <- rep.int(TRUE, n)
          if (path == "na") expected[2L] <- if (na == "base") NA else FALSE
          expected <- switch(type, logical = expected, raw = as.raw(expected),
                             which = which(expected))
          expect_identical(actual, expected)
          expect_identical(calls, rep.int(1L, 3L))
        }
      }
    }
  }
})

# Both parsed predicates and unary calls use cached values too. Resolve an
# unsupported operator in its caller's environment, preserving named args.
local({
  for (fun in c("and3s", "or3s")) {
    calls <- integer(3L)
    lhs <- function() { calls[1L] <<- calls[1L] + 1L; rep(TRUE, 1002L) }
    rhs <- function() { calls[2L] <<- calls[2L] + 1L; rep(FALSE, 1002L) }
    custom <- function(x, y) { calls[3L] <<- calls[3L] + 1L; x == y }
    actual <- eval(substitute(FUN(custom(y = TRUE, x = lhs()), !rhs()),
                              list(FUN = as.name(fun))))
    expect_identical(actual, rep(TRUE, 1002L))
    expect_identical(calls, rep.int(1L, 3L))
  }
})

# Validation of the rest of an NA-containing chain must retain its result
# instead of evaluating third/fourth predicates for a second time.
local({
  for (fun in c("and3s", "or3s")) {
    for (na in c("base", "false")) {
      for (mode in c("strict", "error")) {
        calls <- integer(4L)
        operand <- function(i) {
          calls[i] <<- calls[i] + 1L
          x <- rep.int(1L, 1002L)
          if (i == 1L) x[2L] <- NA_integer_
          x
        }
        actual <- eval(substitute(FUN(operand(1L) == 1L, operand(2L) == 1L,
                                      operand(3L) == 1L, operand(4L) == 1L,
                                      na = NA_MODE, recycle = RECYCLE,
                                      unsupported = UNSUPPORTED),
                                  list(FUN = as.name(fun), NA_MODE = na,
                                       RECYCLE = if (mode == "strict") "strict" else "base",
                                       UNSUPPORTED = if (mode == "error") "error" else "fallback")))
        expected <- rep(TRUE, 1002L)
        if (fun == "and3s") expected[2L] <- if (na == "base") NA else FALSE
        expect_identical(actual, expected)
        expect_identical(calls, rep.int(1L, 4L))
      }
    }
  }
})

# Membership preprocessing replaces xx/yy; fallback must still evaluate the
# original predicate against its original (already evaluated) operands.
local({
  for (fun in c("and3s", "or3s")) {
    calls <- integer(2L)
    lhs <- function() { calls[1L] <<- calls[1L] + 1L; rep(1L, 1002L) }
    table <- function() { calls[2L] <<- calls[2L] + 1L; 1L }
    identity <- fun == "and3s"
    actual <- eval(substitute(FUN(lhs() %in% table(), ID),
                              list(FUN = as.name(fun), ID = identity)))
    expect_identical(actual, rep(TRUE, 1002L))
    expect_identical(calls, c(1L, 1L))
  }
})

# Summing wrappers must evaluate local operators/operands in their caller,
# including a strict NA fallback that recursively validates later operands.
local({
  for (fun in c("sum_and3s", "sum_or3s")) {
    for (na in c("C", "false", "base")) {
      run <- function() {
        calls <- integer(3L)
        lhs <- function() { calls[1L] <<- calls[1L] + 1L; rep(1L, 1002L) }
        rhs <- function() { calls[2L] <<- calls[2L] + 1L; 1L }
        custom <- function(x, y) { calls[3L] <<- calls[3L] + 1L; x == y }
        identity <- fun == "sum_and3s"
        value <- eval(substitute(FUN(custom(lhs(), rhs()), ID, na = NA_MODE),
                                 list(FUN = as.name(fun), ID = identity, NA_MODE = na)))
        list(value, calls)
      }
      actual <- run()
      expect_equal(actual[[1L]], 1002L)
      expect_identical(actual[[2L]], rep.int(1L, 3L))
    }
    calls <- integer(4L)
    operand <- function(i) {
      calls[i] <<- calls[i] + 1L
      x <- rep(1L, 1002L)
      if (i == 1L) x[2L] <- NA_integer_
      x
    }
    actual <- eval(substitute(FUN(operand(1L) == 1L, operand(2L) == 1L,
                                  operand(3L) == 1L, operand(4L) == 1L,
                                  na = "false", recycle = "strict"),
                              list(FUN = as.name(fun))))
    expect_equal(actual, if (fun == "sum_and3s") 1001L else 1002L)
    expect_identical(calls, rep.int(1L, 4L))
  }
})

# Explicit frame forwarding must use the same environment for cached call
# operators as it uses for operand extraction.
local({
  calls <- 0L
  tick <- function() { calls <<- calls + 1L; rep(1L, 1002L) }
  custom <- function(x, y) x == y
  forward <- function() {
    tick <- custom <- function(...) stop("wrong caller environment")
    and3s(custom(tick(), 1L), TRUE, .parent_nframes = 2L)
  }
  expect_identical(forward(), rep(TRUE, 1002L))
  expect_identical(calls, 1L)
})
