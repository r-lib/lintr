#' @name lintr-deprecated
#' @title Deprecated functions in lintr
#'
#' @description
#'
#' These functions have been deprecated from lintr.
#'
#' @seealso [linters] for a complete list of linters available in lintr.
#' @evalRd rd_tags("single_quotes_linter")
#' @keywords internal
NULL

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
