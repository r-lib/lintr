#' Require usage of expect_true(x) over expect_equal(x, TRUE)
#'
#' [testthat::expect_true()] and [testthat::expect_false()] exist specifically
#'   for testing the `TRUE`/`FALSE` value of an object.
#'   [testthat::expect_equal()] and [testthat::expect_identical()] can also be
#'   used for such tests, but it is better to use the tailored function instead.
#'
#' @evalRd rd_tags("expect_true_false_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_true_false_linter <- function() {
  xpath <- "//expr[expr[
    SYMBOL_FUNCTION_CALL[text() = 'expect_equal' or text() = 'expect_identical']
    and following-sibling::expr[position() <= 2 and NUM_CONST[text() = 'TRUE' or text() = 'FALSE']]
  ]]"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    # NB: use expr/$node, not expr[$node], to exclude other things (especially ns:: parts of the call)
    call_name <- xp_call_name(bad_expr, condition = "starts-with(text(), 'expect_')")
    truth_value <- xml2::xml_find_chr(bad_expr, "string(expr/NUM_CONST[text() = 'TRUE' or text() = 'FALSE'])")
    lint_message <- sprintf(
      "expect_%s(x) is better than %s(x, %s)",
      tolower(truth_value), call_name, truth_value
    )

    xml_nodes_to_lints(bad_expr, source_expression, lint_message, type = "warning")
  })
}
