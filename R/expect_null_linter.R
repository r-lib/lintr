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
#' @examples
#' # will produce lints
#' lint(
#'   text = "expect_equal(x, NULL)",
#'   linters = expect_null_linter()
#' )
#'
#' lint(
#'   text = "expect_identical(x, NULL)",
#'   linters = expect_null_linter()
#' )
#'
#' lint(
#'   text = "expect_true(is.null(x))",
#'   linters = expect_null_linter()
#' )
#'
#'
#' # okay
#' lint(
#'   text = "expect_null(x)",
#'   linters = expect_null_linter()
#' )
#'
#' @evalRd rd_tags("expect_null_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_null_linter <- function() {
  # two cases two match:
  #  (1) expect_{equal,identical}(x, NULL) (or NULL, x)
  #  (2) expect_true(is.null(x))
  expect_equal_identical_xpath <- "
  following-sibling::expr[position() <= 2 and NULL_CONST]
    /parent::expr
  "
  expect_true_xpath <- "
  following-sibling::expr[1][expr[1]/SYMBOL_FUNCTION_CALL[text() = 'is.null']]
    /parent::expr
  "

  Linter(linter_level = "expression", function(source_expression) {
    expect_equal_identical_calls <- source_expression$xml_find_function_calls(c("expect_equal", "expect_identical"))
    expect_true_calls <- source_expression$xml_find_function_calls("expect_true")

    bad_expr <- combine_nodesets(
      xml_find_all(expect_equal_identical_calls, expect_equal_identical_xpath),
      xml_find_all(expect_true_calls, expect_true_xpath)
    )

    matched_function <- xp_call_name(bad_expr)
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
