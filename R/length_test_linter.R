#' Check for a common mistake where a size check like 'length' is applied in the wrong place
#'
#' Usage like `length(x == 0)` is a mistake. If you intended to check `x` is empty,
#'   use `length(x) == 0`. Other mistakes are possible, but running `length()` on the
#'   outcome of a logical comparison is never the best choice.
#'
#' The linter also checks for similar usage with `nrow()`, `ncol()`, `NROW()`, and `NCOL()`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "length(x == 0)",
#'   linters = length_test_linter()
#' )
#'
#' lint(
#'   text = "nrow(x > 0) || ncol(x > 0)",
#'   linters = length_test_linter()
#' )
#'
#' lint(
#'   text = "NROW(x == 1) && NCOL(y == 1)",
#'   linters = length_test_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "length(x) > 0",
#'   linters = length_test_linter()
#' )
#'
#' lint(
#'   text = "nrow(x) > 0 || ncol(x) > 0",
#'   linters = length_test_linter()
#' )
#'
#' lint(
#'   text = "NROW(x) == 1 && NCOL(y) == 1",
#'   linters = length_test_linter()
#' )
#'
#' @evalRd rd_tags("class_equals_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
length_test_linter <- function() {
  xpath <- glue::glue("
  following-sibling::expr[{ xp_or(infix_metadata$xml_tag[infix_metadata$comparator]) }]
    /parent::expr
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls(c("length", "nrow", "ncol", "NROW", "NCOL"))
    bad_expr <- xml_find_all(xml_calls, xpath)

    matched_function <- xp_call_name(bad_expr)
    expr_parts <- vapply(lapply(bad_expr, xml_find_all, "expr[2]/*"), xml_text, character(3L))
    lint_message <- sprintf(
      "Checking the %s of a logical vector is likely a mistake. Did you mean `%s(%s) %s %s`?",
      matched_function, matched_function, expr_parts[1L, ], expr_parts[2L, ], expr_parts[3L, ]
    )

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )
  })
}
