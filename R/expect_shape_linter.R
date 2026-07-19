#' Require usage of `expect_shape(x, ...)` over `expect_equal(nrow(x), n)` and similar
#'
#' [testthat::expect_shape()] exists specifically for testing the [nrow()], [ncol()],
#'   or [dim()] of an object. [testthat::expect_equal()] and
#'   [testthat::expect_identical()] can also be used for such tests,
#'   but it is better to use the tailored function instead.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "expect_equal(nrow(x), 4L)",
#'   linters = expect_shape_linter()
#' )
#'
#' lint(
#'   text = "expect_equal(dim(x), c(2L, 3L))",
#'   linters = expect_shape_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "expect_shape(x, nrow = 4L)",
#'   linters = expect_shape_linter()
#' )
#'
#' lint(
#'   text = "expect_shape(x, dim = c(2L, 3L))",
#'   linters = expect_shape_linter()
#' )
#'
#' @evalRd rd_tags("expect_shape_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_shape_linter <- function() {
  shape_funs <- xp_text_in_table(c("dim", "ncol", "nrow"))
  xpath <- glue("
  following-sibling::expr[
    expr[1][SYMBOL_FUNCTION_CALL[{shape_funs}]]
    and (
      position() = 1
      or preceding-sibling::expr[NUM_CONST or OP-COLON or expr[1][SYMBOL_FUNCTION_CALL[text() = 'c']]]
    )
  ]
    /parent::expr[not(SYMBOL_SUB[text() = 'info' or contains(text(), 'label')])]
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls(c("expect_equal", "expect_identical"))
    bad_expr <- xml_find_all_(xml_calls, xpath)

    matched_function <- xp_call_name(bad_expr)
    shape_function <- xp_call_name(bad_expr, depth = 2L, condition = shape_funs)
    shape_arg_var <- ifelse(shape_function == "dim", "d", "n")
    lint_message <- sprintf(
      "expect_shape(x, %s = %s) is better than %s(%s(x), %s)",
      shape_function, shape_arg_var, matched_function, shape_function, shape_arg_var
    )
    xml_nodes_to_lints(bad_expr, source_expression, lint_message, type = "warning")
  })
}
