#' Require usage of `expect_named(x, n)` over `expect_equal(names(x), n)`
#'
#' [testthat::expect_named()] exists specifically for testing the [names()] of
#'   an object. [testthat::expect_equal()] can also be used for such tests,
#'   but it is better to use the tailored function instead.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = 'expect_equal(names(x), "a")',
#'   linters = expect_named_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'expect_named(x, "a")',
#'   linters = expect_named_linter()
#' )
#'
#' lint(
#'   text = 'expect_equal(colnames(x), "a")',
#'   linters = expect_named_linter()
#' )
#'
#' lint(
#'   text = 'expect_equal(dimnames(x), "a")',
#'   linters = expect_named_linter()
#' )
#'
#' @evalRd rd_tags("expect_named_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_named_linter <- function() {
  xpath <- "
  following-sibling::expr[
    expr[1][SYMBOL_FUNCTION_CALL[text() = 'names']]
    and (position() = 1 or preceding-sibling::expr[STR_CONST])
  ]
    /parent::expr
  "

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls(c("expect_equal", "expect_identical"))
    bad_expr <- xml_find_all(xml_calls, xpath)
    matched_function <- xp_call_name(bad_expr)
    lint_message <- sprintf("expect_named(x, n) is better than %s(names(x), n)", matched_function)

    xml_nodes_to_lints(bad_expr, source_expression = source_expression, lint_message, type = "warning")
  })
}
