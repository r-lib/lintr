#' Parentheses before brace linter
#'
#' Check that there is a space between right parentheses and an opening curly brace.
#'
#' @evalRd rd_tags("paren_brace_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
paren_brace_linter <- function() {
  lintr_deprecated("paren_brace_linter", new = "brace_linter", version = "2.0.1.9001", type = "Linter")
  Linter(function(source_file) {
    if (is.null(source_file$xml_parsed_content)) {
      return(NULL)
    }

    xml <- source_file$xml_parsed_content

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
      source_file = source_file,
      lint_message = "There should be a space between right parenthesis and an opening curly brace.",
      type = "style"
    )
  })
}
