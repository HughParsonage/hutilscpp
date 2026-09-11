# R CMD check runs this file in its own R process. Keep hutilscpp unattached:
# attaching it in tinytest hides failures to resolve recursive helper names.
stopifnot(!"package:hutilscpp" %in% search())

local({
  for (n in c(10L, 2000L)) {
    a <- rep_len(c(TRUE, FALSE), n)
    b <- rep_len(c(FALSE, TRUE, TRUE), n)
    c <- rep_len(c(TRUE, TRUE, FALSE), n)
    d <- rep_len(c(FALSE, TRUE, FALSE, TRUE), n)
    for (na in c("C", "false", "base")) {
      for (type in c("logical", "raw", "which")) {
        if (na == "base" && type == "raw") next
        convert <- function(x) switch(type, logical = x, raw = as.raw(x), which = which(x))
        stopifnot(
          identical(hutilscpp::and3s(a, b, c, na = na, type = type), convert(a & b & c)),
          identical(hutilscpp::or3s(a, b, c, na = na, type = type), convert(a | b | c)),
          identical(hutilscpp::and3s(a, b, c, d, na = na, type = type), convert(a & b & c & d)),
          identical(hutilscpp::or3s(a, b, c, d, na = na, type = type), convert(a | b | c | d)),
          identical(hutilscpp::or3s(a, exprC = c, na = na, type = type), convert(a | c)),
          identical(hutilscpp::or3s(a, , c, d, na = na, type = type), convert(a | c | d))
        )
      }
      # Cover every missing-argument branch in the summing wrappers.
      stopifnot(
        hutilscpp::sum_and3s(a, na = na) == sum(a),
        hutilscpp::sum_and3s(a, b, na = na) == sum(a & b),
        hutilscpp::sum_and3s(a, exprC = c, na = na) == sum(a & c),
        hutilscpp::sum_and3s(a, b, c, d, na = na) == sum(a & b & c & d),
        hutilscpp::sum_or3s(a, na = na) == sum(a),
        hutilscpp::sum_or3s(a, b, c, d, na = na) == sum(a | b | c | d)
      )
    }
  }

  # Local functions must not intercept namespace-internal recursion.
  and3s <- or3s <- function(...) stop("called a masked helper")
  stopifnot(
    identical(hutilscpp::and3s(a, b, c), a & b & c),
    identical(hutilscpp::or3s(a, b, c), a | b | c),
    hutilscpp::sum_and3s(a, b, c) == sum(a & b & c),
    hutilscpp::sum_or3s(a, b, c) == sum(a | b | c)
  )

  # Strict NA validation recurses through later predicates too. Preserve
  # caller-local operands and evaluate each side effect exactly once.
  for (fun in c("and3s", "or3s", "sum_and3s", "sum_or3s")) {
    for (na in c("base", "false")) {
      for (mode in c("strict", "error")) {
        calls <- integer(4L)
        operand <- function(i) {
          calls[i] <<- calls[i] + 1L
          x <- rep.int(1L, 2000L)
          if (i == 1L) x[2L] <- NA_integer_
          x
        }
        expr <- substitute(FUN(operand(1L) == 1L, operand(2L) == 1L,
                               operand(3L) == 1L, operand(4L) == 1L,
                               na = NA_MODE, recycle = RECYCLE,
                               unsupported = UNSUPPORTED),
                           list(FUN = call("::", as.name("hutilscpp"), as.name(fun)),
                                NA_MODE = na,
                                RECYCLE = if (mode == "strict") "strict" else "base",
                                UNSUPPORTED = if (mode == "error") "error" else "fallback"))
        actual <- eval(expr)
        expected <- rep(TRUE, 2000L)
        if (fun %in% c("and3s", "sum_and3s")) {
          expected[2L] <- if (na == "base") NA else FALSE
        }
        if (startsWith(fun, "sum_")) expected <- sum(expected)
        stopifnot(isTRUE(all.equal(actual, expected)), identical(calls, rep.int(1L, 4L)))
      }
    }
  }
})

stopifnot(!"package:hutilscpp" %in% search())
