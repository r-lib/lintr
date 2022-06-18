#' Require usage of expect_named(x, n) over expect_equal(names(x), n)
#'
#' [testthat::expect_named()] exists specifically for testing the [names()] of
#'   an object. [testthat::expect_equal()] can also be used for such tests,
#'   but it is better to use the tailored function instead.
#'
#' @evalRd rd_tags("expect_named_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_named_linter <- function() {
  xpath <- "//expr[
    SYMBOL_FUNCTION_CALL[text() = 'expect_equal' or text() = 'expect_identical']
    and following-sibling::expr[
      expr[1][SYMBOL_FUNCTION_CALL[text() = 'names']]
      and (position() = 1 or preceding-sibling::expr[STR_CONST])
    ]
  ]"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)
    matched_function <- xp_call_name(bad_expr, depth = 0L)
    lint_message <- sprintf("expect_named(x, n) is better than %s(names(x), n)", matched_function)

    xml_nodes_to_lints(bad_expr, source_expression = source_expression, lint_message, type = "warning")
  })
}
