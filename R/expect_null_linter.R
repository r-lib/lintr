#' Require usage of `expect_null` for checking `NULL`
#'
#' Require usage of `expect_null(x)` over `expect_equal(x, NULL)` and similar
#' usages.
#'
#' [testthat::expect_null()] exists specifically for testing for `NULL` objects.
#' [testthat::expect_equal()], [testthat::expect_identical()], and
#' [testthat::expect_true()] can also be used for such tests,
#' but it is better to use the tailored function instead.
#'
#' @evalRd rd_tags("expect_null_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_null_linter <- function() {
  # two cases two match:
  #  (1) expect_{equal,identical}(x, NULL) (or NULL, x)
  #  (2) expect_true(is.null(x))
  xpath <- "//expr[
    (
      SYMBOL_FUNCTION_CALL[text() = 'expect_equal' or text() = 'expect_identical']
      and following-sibling::expr[position() <= 2 and NULL_CONST]
    ) or (
      SYMBOL_FUNCTION_CALL[text() = 'expect_true']
      and following-sibling::expr[1][expr[1][SYMBOL_FUNCTION_CALL[text() = 'is.null']]]
    )
  ]"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    matched_function <- xp_call_name(bad_expr, depth = 0L)
    msg <- ifelse(
      matched_function %in% c("expect_equal", "expect_identical"),
      sprintf("expect_null(x) is better than %s(x, NULL)", matched_function),
      "expect_null(x) is better than expect_true(is.null(x))"
    )
    xml_nodes_to_lints(
      bad_expr,
      source_expression,
      lint_message = msg,
      type = "warning"
    )
  })
}
