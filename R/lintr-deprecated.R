#' @name lintr-deprecated
#' @title Deprecated functions in lintr
#'
#' @description
#'
#' These functions have been deprecated from lintr.
#'
#' - `open_curly_linter()` and `closed_curly_linter()` check that open and closed curly braces
#'   are on their own line unless they follow an else, a comma, or a closing bracket.
#'   Deprecated in favor of [brace_linter()].
#'
#' - `paren_brace_linter()` checks that there is a space between right parentheses and an opening
#'    curly brace. E.g., `function(){}` doesn't have a space, while `function() {}` does.
#'    Deprecated in favor of [brace_linter()].
#'
#' - `semicolon_terminator_linter()` checks that no semicolons terminate expressions.
#'    Deprecated in favor of [semicolon_linter()].
#'
#' @param allow_single_line if `TRUE`, allow an open and closed curly pair on the same line.
#' @param semicolon A character vector defining which semicolons to report:
#' \describe{
#'   \item{compound}{Semicolons that separate two statements on the same line.}
#'   \item{trailing}{Semicolons following the last statement on the line.}
#' }
#'
#' @seealso [linters] for a complete list of linters available in lintr.
#' @evalRd rd_tags("closed_curly_linter")
#' @keywords internal
NULL

#' Closed curly linter
#' @rdname lintr-deprecated
#' @export
closed_curly_linter <- function(allow_single_line = FALSE) {
  lintr_deprecated("closed_curly_linter", new = "brace_linter", version = "3.0.0", type = "Linter", signal = "stop")
}

#' Open curly linter
#' @rdname lintr-deprecated
#' @export
open_curly_linter <- function(allow_single_line = FALSE) {
  lintr_deprecated("open_curly_linter", new = "brace_linter", version = "3.0.0", type = "Linter", signal = "stop")
}

#' Parentheses before brace linter
#' @rdname lintr-deprecated
#' @export
paren_brace_linter <- function() {
  lintr_deprecated("paren_brace_linter", new = "brace_linter", version = "3.0.0", type = "Linter", signal = "stop")
}

#' Semicolon linter
#' @rdname lintr-deprecated
#' @export
semicolon_terminator_linter <- function(semicolon = c("compound", "trailing")) {
  lintr_deprecated(
    old = "semicolon_terminator_linter",
    new = "semicolon_linter",
    version = "3.0.0",
    type = "Linter",
    signal = "stop"
  )
}

#' Unnecessary concatenation linter
#' @rdname lintr-deprecated
#' @export
unneeded_concatenation_linter <- function(allow_single_expression = TRUE) {
  lintr_deprecated(
    old = "unneeded_concatenation_linter",
    new = "unnecessary_concatenation_linter",
    version = "3.1.0",
    type = "Linter"
  )

  stopifnot(
    is.logical(allow_single_expression),
    length(allow_single_expression) == 1L
  )
  unnecessary_concatenation_linter(allow_single_expression = allow_single_expression)
}

#' Single quotes linter
#' @rdname lintr-deprecated
#' @export
single_quotes_linter <- function() {
  lintr_deprecated(
    old = "single_quotes_linter",
    new = "quotes_linter",
    version = "3.1.0",
    type = "Linter"
  )
  quotes_linter()
}

#' Consecutive stopifnot linter
#' @rdname lintr-deprecated
#' @export
consecutive_stopifnot_linter <- function() {
  lintr_deprecated(
    old = "consecutive_stopifnot_linter",
    new = "consecutive_assertion_linter",
    version = "3.1.0",
    type = "Linter"
  )
  consecutive_assertion_linter()
}

#' No tabs linter
#' @rdname lintr-deprecated
#' @export
no_tab_linter <- function() {
  lintr_deprecated(
    old = "no_tab_linter",
    new = "whitespace_linter",
    version = "3.1.0",
    type = "Linter"
  )
  whitespace_linter()
}
