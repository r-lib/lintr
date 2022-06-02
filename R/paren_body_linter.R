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
  # careful to do recursive search to the less common OP-RIGHT-PAREN
  #   and forcond nodes (vs. //expr) for performance -- there can
  #   be O(100K) <expr> nodes but in all but pathological examples,
  #   these other nodes will only be a small fraction of this amount.
  # note also that <forcond> only has one following-sibling::expr.
  xpath <- "//OP-RIGHT-PAREN[
    @col1 = following-sibling::expr/@col1 - 1
  ]/following-sibling::expr[
    @line1 = preceding-sibling::FUNCTION/@line1
    or @line1 = preceding-sibling::IF/@line1
    or @line1 = preceding-sibling::WHILE/@line1
    or @line1 = preceding-sibling::OP-LAMBDA/@line1
  ]
  |
  //forcond[
    @line1 = following-sibling::expr/@line1
    and OP-RIGHT-PAREN/@col1 = following-sibling::expr/@col1 - 1
  ]/following-sibling::expr
  "

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content
    matched_expressions <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      matched_expressions,
      source_expression = source_expression,
      lint_message = "There should be a space between right parenthesis and a body expression."
    )
  })
}
