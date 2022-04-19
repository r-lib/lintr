# Helpers for lintr tests

single_quote <- function(x) paste0("'", x, "'")
double_quote <- function(x) paste0('"', x, '"')

#' Trim some leading whitespace
#'
#' Trim `num` characters from the start of every line in `x`, or auto-detect to remove the maximum number whitespace
#' from all lines while preserving relative indentation
#'
#' @param x a string containing newlines
#' @param num number of characters to strip from the start of each line.
#' `NULL` will auto-detect this based on the minimum number of leading spaces greater than one.
#'
#' @return A modified version of `x` with `num` characters removed from the start of every line and with a possible
#' leading and trailing blank line removed.
#'
#' @examples
#' my_var <- local({
#'   out <- trim_some("
#'     This will be the content
#'     of the file where
#'     only the following line
#'       is indented by two spaces.
#'   ")
#' })
#'
#' stopifnot(identical(
#'   my_var,
#'   "This will be the content\nof the file where\nonly the following line\n  is indented by two spaces."
#' ))
trim_some <- function(x, num = NULL) {
  x <- rex::re_substitutes(
    x,
    rex::rex(list(start, any_blanks, newline) %or% list(newline, any_blanks, end)),
    replacement = "",
    global = TRUE
  )

  if (is.null(num)) {
    ms <- rex::re_matches(x, "^\\s+", locations = TRUE, global = TRUE, options = "multi-line")[[1L]]
    num <- min(ms$end - ms$start) + 1L
  }

  rex::re_substitutes(x, rex::rex(start, n_times(any, num)), "", global = TRUE, options = "multi-line")
}

skip_if_not_r_version <- function(min_version) {
  if (getRVersion() < min_version) {
    skip(paste("R version at least", min_version, "is required"))
  }
}
