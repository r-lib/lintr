#' Check for a common mistake where length is applied in the wrong place
#'
#' Usage like `length(x == 0)` is a mistake. If checking the length of `x`,
#'   `length(x) == 0` was intended. The length of the logical vector `x == 0`
#'   will be the same as the length of `x`, so it would be more direct to use
#'   `length(x)` if that value was really intended. Even in more complicated
#'   cases like `length(x == y)` where `x` and `y` may have different lengths
#'   and this expression relies on recycling rules, it is far preferable to
#'   make the intent clearer, e.g. `max(length(x), length(y))`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "length(x == 0)",
#'   linters = length_test_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "length(x) > 0",
#'   linters = length_test_linter()
#' )
#' @evalRd rd_tags("class_equals_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
length_test_linter <- function() {
  xpath <- glue::glue("
  //SYMBOL_FUNCTION_CALL[text() = 'length']
    /parent::expr
    /following-sibling::expr[{ xp_or(infix_metadata$xml_tag[infix_metadata$comparator]) }]
    /parent::expr
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)
    expr_parts <- vapply(lapply(bad_expr, xml_find_all, "expr[2]/*"), xml_text, character(3L))
    lint_message <- sprintf(
      "Checking the length of a logical vector is likely a mistake. Did you mean `length(%s) %s %s`?",
      expr_parts[1L, ], expr_parts[2L, ], expr_parts[3L, ]
    )

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )
  })
}
