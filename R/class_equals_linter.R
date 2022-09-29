#' Block comparison of class with `==`
#'
#' Usage like `class(x) == "character"` is prone to error since class in R
#'   is in general a vector. The correct version for S3 classes is [inherits()]:
#'   `inherits(x, "character")`. Often, class `k` will have an `is.` equivalent,
#'   for example [is.character()] or [is.data.frame()].
#'
#' Similar reasoning applies for `class(x) %in% "character"`
#'
#' @evalRd rd_tags("class_equals_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
class_equals_linter <- function() {
  xpath <- "//expr[
    not(preceding-sibling::OP-LEFT-BRACKET)
    and expr[expr[1][SYMBOL_FUNCTION_CALL[text() = 'class']]]
    and (EQ or NE or SPECIAL[text() = '%in%'])
  ]"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    operator <- xml2::xml_find_chr(bad_expr, "string(*[2])")
    lint_message <- sprintf(
      "Instead of comparing class(x) with %s, use inherits(x, 'class-name') or is.<class> or is(x, 'class')",
      operator
    )
    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )
  })
}
