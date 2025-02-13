#' Require usage of `expect_type(x, type)` over `expect_equal(typeof(x), type)`
#'
#' [testthat::expect_type()] exists specifically for testing the storage type
#'   of objects. [testthat::expect_equal()], [testthat::expect_identical()], and
#'   [testthat::expect_true()] can also be used for such tests,
#'   but it is better to use the tailored function instead.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = 'expect_equal(typeof(x), "double")',
#'   linters = expect_type_linter()
#' )
#'
#' lint(
#'   text = 'expect_identical(typeof(x), "double")',
#'   linters = expect_type_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'expect_type(x, "double")',
#'   linters = expect_type_linter()
#' )
#'
#' @evalRd rd_tags("expect_type_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_type_linter <- function() {
  # NB: the full list of values that can arise from `typeof(x)` is available
  #   in ?typeof (or, slightly more robustly, in the R source: src/main/util.c.
  #   Not all of them are available in is.<type> form, e.g. 'any' or
  #   'special'. 'builtin' and 'closure' are special cases, corresponding to
  #   is.primitive and is.function (essentially).
  base_types <- c(
    "raw", "logical", "integer", "double", "complex", "character", "list",
    "numeric", "function", "primitive", "environment", "pairlist", "promise",
    # Per ?is.language, it's the same as is.call || is.name || is.expression.
    #   so by blocking it, we're forcing more precise tests of one of
    #   those directly ("language", "symbol", and "expression", resp.)
    # NB: is.name and is.symbol are identical.
    "language", "call", "name", "symbol", "expression"
  )
  base_type_tests <- xp_text_in_table(paste0("is.", base_types))
  expect_equal_identical_xpath <- "
  following-sibling::expr[
    expr[1][SYMBOL_FUNCTION_CALL[text() = 'typeof']]
    and (position() = 1 or preceding-sibling::expr[STR_CONST])
  ]
    /parent::expr[not(SYMBOL_SUB[text() = 'info' or text() = 'label' or text() = 'expected.label'])]
  "
  expect_true_xpath <- glue("
  following-sibling::expr[1][expr[1][SYMBOL_FUNCTION_CALL[ {base_type_tests} ]]]
    /parent::expr[not(SYMBOL_SUB[text() = 'info' or text() = 'label'])]
  ")

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
      sprintf("expect_type(x, t) is better than %s(typeof(x), t)", matched_function),
      "expect_type(x, t) is better than expect_true(is.<t>(x))"
    )
    xml_nodes_to_lints(
      bad_expr,
      source_expression,
      lint_message = msg,
      type = "warning"
    )
  })
}
