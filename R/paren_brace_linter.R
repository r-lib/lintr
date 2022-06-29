#' Parentheses before brace linter
#'
#' Check that there is a space between right parentheses and an opening curly brace.
#'
#' @evalRd rd_tags("paren_brace_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
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

    match_exprs <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      match_exprs,
      source_expression = source_expression,
      lint_message = "There should be a space between right parenthesis and an opening curly brace.",
      type = "style"
    )
  })
}
