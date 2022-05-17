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
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression", require_xml = TRUE)) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    xpath <- "//expr[expr[
      SYMBOL_FUNCTION_CALL[text() = 'expect_equal' or text() = 'expect_identical']
      and following-sibling::expr[position() <= 2 and NUM_CONST[text() = 'TRUE' or text() = 'FALSE']]
    ]]"

    bad_expr <- xml2::xml_find_all(xml, xpath)
    xml_nodes_to_lint(
      bad_expr,
      source_expression,
      function(expr) {
        # NB: use expr/$node, not expr[$node], to exclude other things (especially ns:: parts of the call)
        call_name <- xml2::xml_text(
          xml2::xml_find_first(expr, "expr/SYMBOL_FUNCTION_CALL[starts-with(text(), 'expect_')]")
        )
        truth_value <- xml2::xml_text(xml2::xml_find_first(expr, "expr/NUM_CONST[text() = 'TRUE' or text() = 'FALSE']"))
        if (truth_value == "TRUE") {
          sprintf("expect_true(x) is better than %s(x, TRUE)", call_name)
        } else {
          sprintf("expect_false(x) is better than %s(x, FALSE)", call_name)
        }
      },
      type = "warning"
    )
  })
}
