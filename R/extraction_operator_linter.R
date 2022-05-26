#' Extraction operator linter
#'
#' Check that the `[[` operator is used when extracting a single element from an object, not `[` (subsetting) nor `$`
#' (interactive use).
#'
#' @evalRd rd_tags("extraction_operator_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
extraction_operator_linter <- function() {
  xpath <- "
    //OP-DOLLAR[not(preceding-sibling::expr[1]/SYMBOL[text() = 'self' or text() = '.self'])] |
    //OP-LEFT-BRACKET[not(following-sibling::expr[1]/descendant::*[not(
      self::expr or self::OP-PLUS or self::NUM_CONST or self::STR_CONST or self::NULL_CONST
    )]) and not(following-sibling::OP-COMMA)]
  "

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content
    bad_exprs <- xml2::xml_find_all(xml, xpath)
    msgs <- sprintf("Use `[[` instead of `%s` to extract an element.", xml2::xml_text(bad_exprs))

    xml_nodes_to_lints(
      bad_exprs,
      source_expression = source_expression,
      lint_message = msgs,
      type = "warning"
    )
  })
}
