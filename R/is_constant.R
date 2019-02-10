#' Is a vector constant?
#' @description Efficiently decide whether an atomic vector is constant; that is,
#' contains only one value.
#'
#' Equivalent to
#'
#' \code{data.table::uniqueN(x) == 1L}
#'
#' or
#'
#' \code{forecast::is.constant(x)}
#'
#' @param x An atomic vector.
#'
#' @return \code{TRUE} or \code{FALSE}. Missing values are considered to
#' be the same.
#'
#'
#' @examples
#' library(hutilscpp)
#' library(data.table)
#' N <- 1e9L
#' N <- 1e6  # to avoid long-running examples on CRAN
#' consti <- rep(13L, N)
#' bench_system_time(uniqueN(consti) == 1L)
#' #> process    real
#' #>  5.734s  1.202s
#' bench_system_time(is_constant(consti))
#' #>   process      real
#' #> 437.500ms 437.398ms

#' nonconsti <- c(consti, -1L)
#' bench_system_time(uniqueN(nonconsti) == 1L)
#' #> process    real
#' #> 17.812s  3.348s
#' bench_system_time(is_constant(nonconsti))
#' #>   process      real
#' #> 437.500ms 431.104ms

#' constc <- rep("a", N)
#' bench_system_time(uniqueN(constc) == 1L)
#' #> process    real
#' #> 11.141s  3.580s
#' bench_system_time(is_constant(constc))
#' #> process    real
#' #>  6.000s  6.011s

#' nonconstc <- c(constc, "x")
#' bench_system_time(uniqueN(nonconstc) == 1L)
#' #> process    real
#' #> 22.656s  5.629s
#' bench_system_time(is_constant(nonconstc))
#' #> process    real
#' #>  5.906s  5.907s
#'
#'
#' @export

is_constant <- function(x) {
  if (!is.atomic(x)) {
    stop("`x` was not atomic. ",
         "Such objects are not supported.")
  }
  if (length(x) <= 1L) {
    return(TRUE)
  }
  if (is.logical(x)) {
    if (anyNA(x)) {
      if (any(x, na.rm = TRUE)) {
        return(FALSE)
      }
      if (!all(x, na.rm = TRUE)) {
        return(FALSE)
      }
      return(TRUE)
    }
    return(XOR(!any(x, na.rm = TRUE), all(x, na.rm = TRUE)))
  }
  # Instead of anyNA(x) we only need to check the first element
  x1 <- x[1L]
  if (anyNA(x1)) {
    return(all(is.na(x)))
  }

  if (is.integer(x)) {
    !AnyWhich_int(x, x1, FALSE, FALSE, FALSE)
  } else if (is.double(x)) {
    !AnyWhich_dbl(x, x1, FALSE, FALSE, FALSE)
  } else if (is.character(x)) {
    # Does any character in x not not match the first?
    # (double negative because the single positive only
    # checks the second element)
    !AnyCharMatch(x, x1, opposite = TRUE)
  } else if (is.factor(x)) {
    is_constant(as.integer(x))
  } else {
    # e.g. raw
    identical(x, rep_len(x1, length(x)))
  }
}



