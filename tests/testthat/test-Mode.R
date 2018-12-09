context("mode")

test_that("Mode", {
  library(fastmatch)

  Mode <- function(x) {
    if (is.logical(x)) {
      if (anyNA(x)) {
        nas <- sum(is.na(x))
        yes <- sum(x, na.rm = TRUE)
        return(c(TRUE, FALSE, NA)[which.max(c(yes, length(x) - yes - nas, nas))])
      } else {
        if (2L * sum(x) > length(x)) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
    }
    if (is.integer(x)) {
      ux <- min(x):max(x)  # if min(x) == max(x), still okay
      return(ux[which.max(tabulate(match(x, ux)))])
    }
    ux <- unique(x)
    if (is.double(x) || is.character(x)) {
      return(ux[which.max(tabulate(fmatch(x, ux)))])
    } else {
      return(ux[which.max(tabulate( match(x, ux)))])
    }
  }
  table <- function(x) {
    data.table::data.table(x)[, .N, by = "x"][order(-N)]
  }

  Mode_DT <- function(x) {
    setDT(list(x = x))[, .N, by = "x"][which.max(N)][["x"]]
  }

  xx <- sample.int(1e2L, replace = TRUE, size = 1e7)
  bench::system_time(Mode(xx))
  bench::system_time(hutilscpp:::ModeInt(xx))
  expect_equal(Mode(xx), ModeInt(xx))
})
