#' Require usage of `expect_length(x, n)` over `expect_equal(length(x), n)`
#'
#' [testthat::expect_length()] exists specifically for testing the [length()] of
#'   an object. [testthat::expect_equal()] can also be used for such tests,
#'   but it is better to use the tailored function instead.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "expect_equal(length(x), 2L)",
#'   linters = expect_length_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "expect_length(x, 2L)",
#'   linters = expect_length_linter()
#' )
#'
#' @evalRd rd_tags("expect_length_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_length_linter <- function() {
  # TODO(michaelchirico): also catch expect_true(length(x) == 1)
  xpath <- sprintf("
  //SYMBOL_FUNCTION_CALL[text() = 'expect_equal' or text() = 'expect_identical']
    /parent::expr
    /following-sibling::expr[
      expr[1][SYMBOL_FUNCTION_CALL[text() = 'length']]
      and (position() = 1 or preceding-sibling::expr[NUM_CONST])
    ]
    /parent::expr[not(SYMBOL_SUB[text() = 'info' or contains(text(), 'label')])]
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)
    matched_function <- xp_call_name(bad_expr)
    lint_message <- sprintf("expect_length(x, n) is better than %s(length(x), n)", matched_function)
    xml_nodes_to_lints(bad_expr, source_expression, lint_message, type = "warning")
  })
}
