#' Check for a common mistake where length is applied in the wrong place
#'
#' Usage like `length(x == 0)` is a mistake. If you intended to check `x` is empty,
#'   use `length(x) == 0`. Other mistakes are possible, but running `length()` on the
#'   outcome of a logical comparison is never the best choice.
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
  following-sibling::expr[{ xp_or(infix_metadata$xml_tag[infix_metadata$comparator]) }]
    /parent::expr
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls("length")
    bad_expr <- xml_find_all(xml_calls, xpath)

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
