#' @name lintr-deprecated
#' @title Deprecated functions in lintr
#'
#' @description
#'
#' These functions have been deprecated from lintr.
#'
#' - `open_curly_linter()` and `closed_curly_linter()` check that open and closed curly braces
#'   are on their own line unless they follow an else, a comma, or a closing bracket.
#'   Deprecated in favor of `brace_linter()`.
#'
#' - `paren_brace_linter()` checks that there is a space between right parentheses and an opening
#'    curly brace. E.g., `function(){}` doesn't have a space, while `function() {}` does.
#'    Deprecated in favor of `brace_linter()`.
#'
#' - `semicolon_terminator_linter()` checks that no semicolons terminate expressions.
#'    Deprecated in favor of `semicolon_linter()`.
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
  lintr_deprecated("closed_curly_linter", new = "brace_linter", version = "3.0.0", type = "Linter")
  xp_cond_closed <- xp_and(c(
    # matching { is on same line
    if (isTRUE(allow_single_line)) {
      "(@line1 != preceding-sibling::OP-LEFT-BRACE/@line1)"
    },
    # immediately followed by ",", "]" or ")"
    "not(
      @line1 = ancestor::expr/following-sibling::*[1][
        self::OP-COMMA or self::OP-RIGHT-BRACKET or self::OP-RIGHT-PAREN
      ]/@line1
    )",
    # double curly
    "not(
      (@line1 = parent::expr/following-sibling::OP-RIGHT-BRACE/@line1) or
      (@line1 = preceding-sibling::expr/OP-RIGHT-BRACE/@line1)
    )"
  ))

  xpath <- glue("//OP-RIGHT-BRACE[
    { xp_cond_closed } and (
      (@line1 = preceding-sibling::*[1]/@line2) or
      (@line1 = parent::expr/following-sibling::*[1][not(self::ELSE)]/@line1)
    )
  ]")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml_nodes_to_lints(
      xml_find_all(source_expression$xml_parsed_content, xpath),
      source_expression = source_expression,
      lint_message = "Closing curly-braces should always be on their own line, unless they are followed by an else."
    )
  })
}

#' Open curly linter
#' @rdname lintr-deprecated
#' @export
open_curly_linter <- function(allow_single_line = FALSE) {
  lintr_deprecated("open_curly_linter", new = "brace_linter", version = "3.0.0", type = "Linter")

  xpath_before <- "//OP-LEFT-BRACE[
    not(following-sibling::expr[1][OP-LEFT-BRACE])
    and not(parent::expr/preceding-sibling::*[1][OP-LEFT-BRACE])
    and @line1 != parent::expr/preceding-sibling::*[1][not(self::ELSE)]/@line2
  ]"
  if (allow_single_line) {
    xpath_after <- "//OP-LEFT-BRACE[
      not(following-sibling::expr[1][OP-LEFT-BRACE])
      and not(parent::expr/preceding-sibling::OP-LEFT-BRACE)
      and not(@line2 = following-sibling::OP-RIGHT-BRACE/@line1)
      and @line2 = following-sibling::expr[position() = 1 and not(OP-LEFT-BRACE)]/@line1
    ]"
    message_after <- paste(
      "Opening curly braces should always be followed by a new line",
      "unless the paired closing brace is on the same line."
    )
  } else {
    xpath_after <- "//OP-LEFT-BRACE[
      not(following-sibling::expr[1][OP-LEFT-BRACE])
      and not(parent::expr/preceding-sibling::OP-LEFT-BRACE)
      and @line2 = following-sibling::expr[1]/@line1
    ]"
    message_after <- "Opening curly braces should always be followed by a new line."
  }

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    expr_before <- xml_find_all(xml, xpath_before)
    lints_before <- xml_nodes_to_lints(
      expr_before,
      source_expression = source_expression,
      lint_message = "Opening curly braces should never go on their own line.",
      type = "style"
    )

    expr_after <- xml_find_all(xml, xpath_after)
    lints_after <- xml_nodes_to_lints(
      expr_after,
      source_expression = source_expression,
      lint_message = message_after,
      type = "style"
    )

    return(c(lints_before, lints_after))
  })
}

#' Parentheses before brace linter
#' @rdname lintr-deprecated
#' @export
paren_brace_linter <- function() {
  lintr_deprecated("paren_brace_linter", new = "brace_linter", version = "3.0.0", type = "Linter")

  xpath <- paste(
    "//OP-LEFT-BRACE[",
    "@line1 = parent::expr/preceding-sibling::OP-RIGHT-PAREN/@line1",
    "and",
    "@col1 = parent::expr/preceding-sibling::OP-RIGHT-PAREN/@col1 + 1",
    "]"
  )

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    match_exprs <- xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      match_exprs,
      source_expression = source_expression,
      lint_message = "There should be a space between right parenthesis and an opening curly brace.",
      type = "style"
    )
  })
}

#' Semicolon linter
#' @rdname lintr-deprecated
#' @export
semicolon_terminator_linter <- function(semicolon = c("compound", "trailing")) {
  lintr_deprecated(
    old = "semicolon_terminator_linter",
    new = "semicolon_linter",
    version = "3.0.0",
    type = "Linter"
  )
  semicolon <- match.arg(semicolon, several.ok = TRUE)
  allow_compound <- !"compound" %in% semicolon
  allow_trailing <- !"trailing" %in% semicolon
  semicolon_linter(allow_compound, allow_trailing)
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
