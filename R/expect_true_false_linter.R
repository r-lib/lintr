#' Require usage of `expect_true(x)` over `expect_equal(x, TRUE)`
#'
#' [testthat::expect_true()] and [testthat::expect_false()] exist specifically
#'   for testing the `TRUE`/`FALSE` value of an object.
#'   [testthat::expect_equal()] and [testthat::expect_identical()] can also be
#'   used for such tests, but it is better to use the tailored function instead.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "expect_equal(x, TRUE)",
#'   linters = expect_true_false_linter()
#' )
#'
#' lint(
#'   text = "expect_equal(x, FALSE)",
#'   linters = expect_true_false_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "expect_true(x)",
#'   linters = expect_true_false_linter()
#' )
#'
#' lint(
#'   text = "expect_false(x)",
#'   linters = expect_true_false_linter()
#' )
#'
#' @evalRd rd_tags("expect_true_false_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_true_false_linter <- function() {
  xpath <- "
  following-sibling::expr[position() <= 2 and NUM_CONST[text() = 'TRUE' or text() = 'FALSE']]
    /parent::expr
  "

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls(c("expect_equal", "expect_identical"))
    bad_expr <- xml_find_all(xml_calls, xpath)

    # NB: use expr/$node, not expr[$node], to exclude other things (especially ns:: parts of the call)
    call_name <- xp_call_name(bad_expr, condition = "starts-with(text(), 'expect_')")
    truth_value <- xml_find_chr(bad_expr, "string(expr/NUM_CONST[text() = 'TRUE' or text() = 'FALSE'])")
    lint_message <- sprintf(
      "expect_%s(x) is better than %s(x, %s)",
      tolower(truth_value), call_name, truth_value
    )

    xml_nodes_to_lints(bad_expr, source_expression, lint_message, type = "warning")
  })
}
