#' @name lintr-deprecated
#' @title Deprecated functions in lintr
#'
#' @seealso [linters] for a complete list of linters available in lintr.
#' @evalRd rd_tags(tags = "deprecated")
#' @keywords internal
NULL

#' Unnecessary concatenation linter
#' @rdname lintr-deprecated
#' @export
unneeded_concatenation_linter <- function(allow_single_expression = TRUE) {
  lintr_deprecated(
    what = "unneeded_concatenation_linter",
    alternative = "unnecessary_concatenation_linter",
    version = "3.1.0",
    type = "Linter",
    signal = "stop"
  )
}

#' Single quotes linter
#' @rdname lintr-deprecated
#' @export
single_quotes_linter <- function() {
  lintr_deprecated(
    what = "single_quotes_linter",
    alternative = "quotes_linter",
    version = "3.1.0",
    type = "Linter",
    signal = "stop"
  )
}

#' Consecutive stopifnot linter
#' @rdname lintr-deprecated
#' @export
consecutive_stopifnot_linter <- function() {
  lintr_deprecated(
    what = "consecutive_stopifnot_linter",
    alternative = "consecutive_assertion_linter",
    version = "3.1.0",
    type = "Linter",
    signal = "stop"
  )
}

#' No tabs linter
#' @rdname lintr-deprecated
#' @export
no_tab_linter <- function() {
  lintr_deprecated(
    what = "no_tab_linter",
    alternative = "whitespace_linter",
    version = "3.1.0",
    type = "Linter",
    signal = "stop"
  )
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
