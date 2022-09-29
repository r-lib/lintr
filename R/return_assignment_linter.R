#' Block assignments inside function returns
#'
#' `return(x <- ...)` is either distracting (because `x` is ignored), or
#'   confusing (because assigning to `x` has some side effect that is muddled
#'   by the dual-purpose expression).
#'
#' @evalRd rd_tags("return_assignment_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
return_assignment_linter <- function() {
  xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'return']
  /parent::expr/parent::expr/expr[LEFT_ASSIGN or RIGHT_ASSIGN]
  "

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = "Move the assignment outside of the return() clause, or skip assignment altogether.",
      type = "warning"
    )
  })
}
