#' @name lintr-deprecated
#' @title Deprecated functions in lintr
#'
#' @description
#'
#' These functions have been deprecated from lintr.
#'
#' - `open_curly_linter()` (use [brace_linter()])
#' - `closed_curly_linter()` (use `brace_linter()`)
#' - `paren_brace_linter()` (use `brace_linter()`)
#' - `semicolon_terminator_linter()` (use [semicolon_linter()])
#'
#' @param allow_single_line,semicolon Irrelevant parameters to defunct linters.
#'
#' @seealso [linters] for a complete list of linters available in lintr.
#' @evalRd rd_tags("single_quotes_linter")
#' @keywords internal
NULL

#' Closed curly linter
#' @rdname lintr-deprecated
#' @export
closed_curly_linter <- function(allow_single_line = FALSE) {
  lintr_deprecated(
    what = "closed_curly_linter",
    alternative = "brace_linter",
    version = "3.0.0",
    type = "Linter",
    signal = "stop"
  )
}

#' Open curly linter
#' @rdname lintr-deprecated
#' @export
open_curly_linter <- function(allow_single_line = FALSE) {
  lintr_deprecated(
    what = "open_curly_linter",
    alternative = "brace_linter",
    version = "3.0.0",
    type = "Linter",
    signal = "stop"
  )
}

#' Parentheses before brace linter
#' @rdname lintr-deprecated
#' @export
paren_brace_linter <- function() {
  lintr_deprecated(
    what = "paren_brace_linter",
    alternative = "brace_linter",
    version = "3.0.0",
    type = "Linter",
    signal = "stop"
  )
}

#' Semicolon linter
#' @rdname lintr-deprecated
#' @export
semicolon_terminator_linter <- function(semicolon = c("compound", "trailing")) {
  lintr_deprecated(
    what = "semicolon_terminator_linter",
    alternative = "semicolon_linter",
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
    what = "unneeded_concatenation_linter",
    alternative = "unnecessary_concatenation_linter",
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
    what = "single_quotes_linter",
    alternative = "quotes_linter",
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
    what = "consecutive_stopifnot_linter",
    alternative = "consecutive_assertion_linter",
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
    what = "no_tab_linter",
    alternative = "whitespace_linter",
    version = "3.1.0",
    type = "Linter"
  )
  whitespace_linter()
}

#' Extraction operator linter
#' @rdname lintr-deprecated
#' @export
extraction_operator_linter <- function() {
  lintr_deprecated(
    what = "extraction_operator_linter",
    version = "3.2.0",
    type = "Linter",
    signal = "stop"
  )
}

#' Unnecessary nested if linter
#' @rdname lintr-deprecated
#' @export
unnecessary_nested_if_linter <- function() {
  lintr_deprecated(
    what = "unnecessary_nested_if_linter",
    alternative = "unnecessary_nesting_linter",
    version = "3.2.0",
    type = "Linter",
    signal = "stop"
  )
}
