#' Parentheses before brace linter
#'
#' Check that there is a space between right parentheses and an opening curly brace.
#'
#' @evalRd rd_tags("paren_brace_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
paren_brace_linter <- function() {
  Linter(function(source_expression) {
    if (is.null(source_expression$xml_parsed_content)) {
      return(NULL)
    }

    xml <- source_expression$xml_parsed_content

    xpath <- paste(
      "//OP-LEFT-BRACE[",
      "@line1 = parent::expr/preceding-sibling::OP-RIGHT-PAREN/@line1",
      "and",
      "@col1 = parent::expr/preceding-sibling::OP-RIGHT-PAREN/@col1 + 1",
      "]"
    )

    match_exprs <- xml2::xml_find_all(xml, xpath)

    lapply(
      match_exprs,
      xml_nodes_to_lint,
      source_expression = source_expression,
      lint_message = "There should be a space between right parenthesis and an opening curly brace.",
      type = "style"
    )
  })
}
