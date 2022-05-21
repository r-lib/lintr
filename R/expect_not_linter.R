#' Require usage of expect_false(.) over expect_true(!.)
#'
#' [testthat::expect_false()] exists specifically for testing that an output is
#'   `FALSE`. [testthat::expect_true()] can also be used for such tests by
#'   negating the output, but it is better to use the tailored function instead.
#'   The reverse is also true -- use `expect_false(A)` instead of
#'   `expect_true(!A)`.
#'
#' @evalRd rd_tags("expect_not_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_not_linter <- function() {
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    xpath <- "//expr[
      SYMBOL_FUNCTION_CALL[text() = 'expect_true' or text() = 'expect_false']
      and following-sibling::expr[1][OP-EXCLAMATION]
    ]"

    bad_expr <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = "expect_false(x) is better than expect_true(!x), and vice versa.",
      type = "warning"
    )
  })
}
