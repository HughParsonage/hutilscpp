

extractMandatory <- function(x, command, nCommands) {
  ans <- .Call("CextractMandatory", x, command, nCommands, PACKAGE = packageName)
  names(ans) <- c("support", "Openers", "Closers")
  ans
}

#' @name where_square_bracket_opens
#' @param x Character vector of characters
#' @param i integer position of closing bracket
#' @return
#' -1 if x[i] is not a closing bracket
#' 0 if bracket never opens
#' j position where x[j] opens x[i];
#' @noRd
where_square_bracket_opens <- function(x, i) {
  .Call("Cwhere_square_bracket_opens", x, i, PACKAGE = packageName)
}

print_chars <- function(x) {
  invisible(.Call("CPrintChars", x, PACKAGE = packageName))  # nocov
}



