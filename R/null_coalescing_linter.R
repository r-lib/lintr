#' Encourage usage of the null coalescing operator `%||%`
#'
#' The `x %||% y` is equivalent to
#'   `if (is.null(x)) y else x`, but more expressive.
#'   It is exported by R since 4.4.0, and equivalents
#'   have been available in other tidyverse packages
#'   for much longer, e.g. 2008 for ggplot2.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "if (is.null(x)) y else x",
#'   linters = null_coalescing_linter()
#' )
#'
#' lint(
#'   text = "if (!is.null(x)) x else y",
#'   linters = null_coalescing_linter()
#' )
#'
#' lint(
#'   text = "if (is.null(x[1])) x[2] else x[1]",
#'   linters = null_coalescing_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x %||% y",
#'   linters = null_coalescing_linter()
#' )
#'
#' lint(
#'   text = "x %||% y",
#'   linters = null_coalescing_linter()
#' )
#'
#' lint(
#'   text = "x[1] %||% x[2]",
#'   linters = null_coalescing_linter()
#' )
#'
#'
#' @evalRd rd_tags("null_coalescing_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
null_coalescing_linter <- function() {
  xpath = "
  parent::expr[(
    preceding-sibling::IF
    and (
      expr[2] = following-sibling::ELSE/following-sibling::expr
      or expr[2][LEFT_ASSIGN]/expr[1] = following-sibling::ELSE/following-sibling::expr
    )
  ) or (
    preceding-sibling::OP-EXCLAMATION
    and parent::expr/preceding-sibling::IF
    and (
      expr[2] = parent::expr/following-sibling::expr[1]
      or expr[2][LEFT_ASSIGN]/expr[1] = parent::expr/following-sibling::expr[1]
    )
  )]"

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)

    is_negation <- !is.na(xml_find_first(bad_expr, "preceding-sibling::OP-EXCLAMATION")

    observed <- ifelse(is_negation, "if (!is.null(x)) x else y", "if (is.null(x)) y else x")

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = paste0("Use x %||% y instead of ", observed, "."),
      type = "warning"
    )
  })
}
