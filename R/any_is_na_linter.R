#' Require usage of anyNA over any(is.na(.))
#'
#' [anyNA()] exists as a replacement for `any(is.na(.))` which is more efficient
#'   for simple objects, and in the worst case is the same efficiency. Therefore
#'   it should be used in all situations instead of the latter.
#'
#' @evalRd rd_tags("any_is_na_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
any_is_na_linter <- function() {
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression", require_xml = TRUE)) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    xpath <- "//expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'any']]
      and expr[expr[SYMBOL_FUNCTION_CALL[text() = 'is.na']]]
      and (
        count(expr) = 2
        or (count(expr) = 3 and SYMBOL_SUB[text() = 'na.rm'])
      )
    ]"

    bad_expr <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = "anyNA(x) is better than any(is.na(x)).",
      type = "warning"
    )
  })
}
