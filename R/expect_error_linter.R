#' Require usage of expect_error(x) where appropriate
#'
#' If code is expected to produce an error, using `expect_that(x,
#' throws_error())` works but is unadvised. It is better to use the function
#' dedicated specifically for checking errors: `expect_error(x)`. The additional
#' benefit is that you can also check for the expected error message using
#' regular expressions.
#'
#' @evalRd rd_tags("expect_error_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_error_linter <- function() {
  # one case to match:
  #  - expect_that(x, throws_error())
  xpath <- glue::glue("//expr[
    (
      SYMBOL_FUNCTION_CALL[text() = 'expect_that']
    and
      following-sibling::expr[expr[1][SYMBOL_FUNCTION_CALL[text() = 'throws_error']]]
    )
  ]")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)
    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = "Use expect_error(x) instead of expect_that(x, throws_error()).",
      type = "warning"
    )
  })
}
