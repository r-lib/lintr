#' Force consecutive calls to assertions into just one when possible
#'
#' [stopifnot()] accepts any number of tests, so sequences like
#'   `stopifnot(x); stopifnot(y)` are redundant. Ditto for tests using
#'   `assertthat::assert_that()` without specifying `msg=`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "stopifnot(x); stopifnot(y)",
#'   linters = consecutive_assertion_linter()
#' )
#'
#' lint(
#'   text = "assert_that(x); assert_that(y)",
#'   linters = consecutive_assertion_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "stopifnot(x, y)",
#'   linters = consecutive_assertion_linter()
#' )
#'
#' lint(
#'   text = 'assert_that(x, msg = "Bad x!"); assert_that(y)',
#'   linters = consecutive_assertion_linter()
#' )
#'
#' @evalRd rd_tags("consecutive_assertion_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
consecutive_assertion_linter <- function() {
  # annoying expr-but-not-really nodes
  next_expr <- "following-sibling::*[self::expr or self::expr_or_assign_or_help or self::equal_assign][1]"

  stopifnot_xpath <- glue("
  //SYMBOL_FUNCTION_CALL[text() = 'stopifnot']
    /parent::expr
    /parent::expr[
      expr[1]/SYMBOL_FUNCTION_CALL = {next_expr}/expr[1]/SYMBOL_FUNCTION_CALL
    ]
  ")
  assert_that_xpath <- glue("
  //SYMBOL_FUNCTION_CALL[text() = 'assert_that']
    /parent::expr
    /parent::expr[
      not(SYMBOL_SUB[text() = 'msg'])
      and not(following-sibling::expr[1]/SYMBOL_SUB[text() = 'msg'])
      and expr[1]/SYMBOL_FUNCTION_CALL = {next_expr}/expr[1]/SYMBOL_FUNCTION_CALL
    ]
  ")

  Linter(function(source_expression) {
    # need the full file to also catch usages at the top level
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    bad_expr <- combine_nodesets(
      xml_find_all(xml, stopifnot_xpath),
      xml_find_all(xml, assert_that_xpath)
    )

    matched_function <- xp_call_name(bad_expr)
    xml_nodes_to_lints(
      bad_expr,
      source_expression,
      lint_message = sprintf("Unify consecutive calls to %s().", matched_function),
      type = "warning"
    )
  })
}
