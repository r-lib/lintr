#' @name lintr-deprecated
#' @title Deprecated functions in lintr
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
    signal = "warning"
  )

  constant_nodes_in_brackets <- paste0("self::", c("expr", "OP-PLUS", "NUM_CONST", "STR_CONST"))
  xpath <- glue("
  //OP-DOLLAR[not(preceding-sibling::expr[1]/SYMBOL[text() = 'self' or text() = '.self'])]
  |
  //OP-LEFT-BRACKET[
    not(following-sibling::expr[1]/descendant::*[not({xp_or(constant_nodes_in_brackets)})]) and
    not(following-sibling::OP-COMMA)
  ]
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_exprs <- xml_find_all(xml, xpath)
    msgs <- sprintf("Use `[[` instead of `%s` to extract an element.", xml_text(bad_exprs))

    xml_nodes_to_lints(
      bad_exprs,
      source_expression = source_expression,
      lint_message = msgs,
      type = "warning"
    )
  })
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
    signal = "warning"
  )

  xpath <- paste0(
    "//IF/parent::expr[not(ELSE)]/OP-RIGHT-PAREN/",
    c(
      "following-sibling::expr[IF and not(ELSE)]", # catch if (cond) if (other_cond) { ... }
      "following-sibling::expr[OP-LEFT-BRACE and count(expr) = 1]
         /expr[IF and not(ELSE)]" # catch if (cond) { if (other_cond) { ... } }
    ),
    collapse = " | "
  )

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_exprs <- xml_find_all(xml, xpath)
    xml_nodes_to_lints(
      bad_exprs,
      source_expression = source_expression,
      lint_message = paste(
        "Don't use nested `if` statements,",
        "where a single `if` with the combined conditional expression will do.",
        "For example, instead of `if (x) { if (y) { ... }}`, use `if (x && y) { ... }`."
      )
    )
  })
}
