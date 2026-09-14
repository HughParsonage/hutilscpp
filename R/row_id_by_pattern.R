#' Row ids by cell pattern
#'
#' @description Assign every row of a data.frame an integer id such that two
#' rows share an id if and only if they have the same *pattern* in every
#' column, where the pattern of a cell is a cheap summary of its value rather
#' than the value itself. Ids are ranked by frequency: `1L` is the most common
#' pattern.
#'
#' @param DT A `data.frame` (typically a `data.table`). It is not modified.
#' @param na_is `0L` or `1L`. The pattern value assigned to `NA` and `NaN` in
#'   integer, logical, and double columns.
#' @param magnitude `TRUE` or `FALSE`. If `FALSE` (the default), integer and
#'   double columns are summarised as zero versus nonzero. If `TRUE`, they are
#'   summarised by sign and `floor(log2(abs(x)))`, so values of the same sign
#'   and binary order of magnitude share a pattern. See Details.
#' @param max_patterns A positive whole number. Only the `max_patterns` most
#'   frequent patterns receive an id; rows belonging to any other pattern
#'   are `NA_integer_`. The default (`.Machine$integer.max`) is the largest
#'   number of ids an integer vector can hold.
#' @param nThread Number of threads to use.
#'
#' @return An integer vector with one element per row of `DT`. Ids are dense
#'   (`1L` to the number of retained patterns), ordered by decreasing
#'   frequency, with ties broken by first appearance in `DT`.
#'
#' @details
#' The pattern of a cell depends on the column type:
#'
#' * **integer, double, logical**: `1L` if nonzero, `0L` if zero.
#'   `NA` and `NaN` take the value `na_is`. `-0` counts as zero.
#' * **raw**: `1L` if nonzero, `0L` if zero.
#' * **factor**: the integer code, so rows are grouped by level. `NA` is its own
#'   pattern.
#' * **character**: the string itself. `NA_character_` is its own pattern.
#'   Strings are compared by identity in R's global string cache, so two
#'   strings that are equal only after translation between encodings are
#'   treated as distinct.
#' * Other classed columns backed by integer or double storage (`Date`,
#'   `IDate`, `POSIXct`, ...) follow their storage type. Columns of class
#'   `integer64` or `nanotime` are not supported, since their `NA` is stored
#'   as a bit pattern that would read as zero.
#'
#' With `magnitude = TRUE` the pattern of an integer or double is one of:
#' zero; a separate category for `NA`/`NaN` when `na_is = 1L` (when
#' `na_is = 0L` they join the zero category); otherwise the pair
#' (`sign(x)`, `floor(log2(abs(x)))`), computed exactly from the binary
#' representation (subnormal doubles included). `Inf` and `-Inf` are their own
#' categories. Logical and raw columns are unaffected by `magnitude`.
#'
#' Rows are compared through a 128-bit hash of their patterns rather than
#' pattern-by-pattern, so distinct patterns could in principle collide. The
#' probability is below `1e-20` for any table that fits in memory.
#'
#' Columns that are ALTREP (for example `1:n`) are expanded before hashing.
#'
#' @examples
#' DT <- data.frame(a = c(0, 1, 2, 0), b = c("x", "y", "y", "x"))
#' row_id_by_pattern(DT)
#' row_id_by_pattern(DT, max_patterns = 1L)
#' row_id_by_pattern(data.frame(x = c(1, 2, 3, 4, -1, 0)), magnitude = TRUE)
#'
#' @export
row_id_by_pattern <- function(DT,
                              na_is = 0L,
                              magnitude = FALSE,
                              max_patterns = .Machine$integer.max,
                              nThread = getOption("hutilscpp.nThread", 1L)) {
  if (!is.data.frame(DT)) {
    stop("`DT` was a ", class(DT)[1L], ", but must be a data.frame.")
  }
  check_TF(magnitude)
  if (res <- isnt_number(na_is, int.only = TRUE)) {
    stop(attr(res, "ErrorMessage"))
  }
  if (na_is != 0 && na_is != 1) {
    stop("`na_is = ", na_is, "` but must be 0 or 1.")
  }
  na_is <- as.integer(na_is)
  if (res <- isnt_number(max_patterns)) {
    stop(attr(res, "ErrorMessage"))
  }
  if (max_patterns < 1 || max_patterns != floor(max_patterns)) {
    stop("`max_patterns = ", max_patterns, "` but must be a positive whole number.")
  }
  max_patterns <- min(as.double(max_patterns), .Machine$integer.max)
  nThread <- check_omp(nThread)

  N <- nrow(DT)
  if (length(DT) == 0L) {
    return(rep(1L, N))
  }
  if (N == 0L) {
    return(integer(0))
  }
  kinds <- vapply(DT, ribp_kind, 0L, magnitude = magnitude, USE.NAMES = FALSE)
  if (anyNA(kinds)) {
    bad <- names(DT)[is.na(kinds)]
    stop("`DT` has column(s) with unsupported types: ",
         toString(paste0("`", bad, "`")), ". ",
         "Only integer, double, logical, raw, factor, and character columns are supported.")
  }
  .Call(Crow_id_by_pattern, DT, kinds, na_is, max_patterns, nThread)
}

# Column kind codes understood by Crow_id_by_pattern:
#   0 int/logical 0/1, 1 double 0/1, 2 raw, 3 factor, 4 character,
#   5 int magnitude, 6 double magnitude. NA for unsupported columns.
ribp_kind <- function(x, magnitude) {
  if (is.factor(x)) {
    return(3L)
  }
  if (inherits(x, c("integer64", "nanotime"))) {
    return(NA_integer_)
  }
  switch(typeof(x),
         logical = 0L,
         integer = if (magnitude) 5L else 0L,
         double = if (magnitude) 6L else 1L,
         raw = 2L,
         character = 4L,
         NA_integer_)
}
