#' Parenthesis before body linter
#'
#' Check that there is a space between right parenthesis and a body expression.
#'
#' @evalRd rd_tags("paren_body_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#parentheses>
#' @export
paren_body_linter <- function() {
  Linter(function(source_expression) {
    if (is.null(source_expression$xml_parsed_content)) return(NULL)

    xpath <- paste(
      "//expr[",
      "@line1 = preceding-sibling::FUNCTION/@line1",
      "|",
      "preceding-sibling::IF/@line1",
      "|",
      "preceding-sibling::WHILE/@line1",
      "|",
      "preceding-sibling::OP-LAMBDA/@line1",
      "and",
      "@col1 = preceding-sibling::OP-RIGHT-PAREN/@col1 + 1",
      "]",
      "|",
      "//expr[",
      "@line1 = preceding-sibling::forcond/@line1",
      "and",
      "@col1 = preceding-sibling::forcond/OP-RIGHT-PAREN/@col1 + 1",
      "]"
    )
    matched_expressions <- xml2::xml_find_all(source_expression$xml_parsed_content, xpath)

    lapply(
      matched_expressions,
      xml_nodes_to_lint,
      source_expression = source_expression,
      lint_message = "There should be a space between right parenthesis and a body expression."
    )
  })
}
