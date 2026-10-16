#' Require usage of `expect_no_*()` over `expect_*(..., NA)`
#'
#' [testthat::expect_no_error()], [testthat::expect_no_warning()],
#'   [testthat::expect_no_message()], and [testthat::expect_no_condition()]
#'   exist specifically for testing the absence of errors, warnings, messages,
#'   or conditions. [testthat::expect_error()], [testthat::expect_warning()],
#'   [testthat::expect_message()], and [testthat::expect_condition()] with
#'   `regexp = NA` can also be used for such tests, but it is better to use the
#'   tailored functions instead.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "expect_error(foo(), NA)",
#'   linters = expect_no_condition_linter()
#' )
#'
#' lint(
#'   text = "expect_warning(foo(), regexp = NA)",
#'   linters = expect_no_condition_linter()
#' )
#'
#' lint(
#'   text = "expect_message(foo(), NA)",
#'   linters = expect_no_condition_linter()
#' )
#'
#' lint(
#'   text = "expect_condition(foo(), NA)",
#'   linters = expect_no_condition_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "expect_no_error(foo())",
#'   linters = expect_no_condition_linter()
#' )
#'
#' lint(
#'   text = "expect_no_warning(foo())",
#'   linters = expect_no_condition_linter()
#' )
#'
#' lint(
#'   text = "expect_no_message(foo())",
#'   linters = expect_no_condition_linter()
#' )
#'
#' lint(
#'   text = "expect_no_condition(foo())",
#'   linters = expect_no_condition_linter()
#' )
#'
#' @evalRd rd_tags("expect_no_condition_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_no_condition_linter <- function() {
  na_cond <- "NUM_CONST[text() = 'NA' or text() = 'NA_character_']"
  not_kwarg_cond <- "not(preceding-sibling::*[not(self::COMMENT)][1][self::EQ_SUB])"
  has_object_cond <- glue("
    parent::expr[
      SYMBOL_SUB[text() = 'object']
      or preceding-sibling::PIPE
      or (
        preceding-sibling::SPECIAL[{xp_text_in_table(magrittr_pipes)}]
        and not(expr[2][SYMBOL[text() = '.']])
      )
    ]
  ")

  na_xpath <- glue("
  parent::expr[not(SYMBOL_SUB[text() = 'info' or contains(text(), 'label')])]
    /expr[
      {na_cond}
      and (
        preceding-sibling::*[not(self::COMMENT)][1][self::EQ_SUB]
          /preceding-sibling::*[not(self::COMMENT)][1][self::SYMBOL_SUB and text() = 'regexp']
        or (
          {not_kwarg_cond}
          and not(parent::expr/SYMBOL_SUB[text() = 'regexp'])
          and count(preceding-sibling::expr[{not_kwarg_cond}]) = 2 - count({has_object_cond})
        )
      )
    ]
  ")

  expect_condition_funs <- c(
    "expect_error",
    "expect_warning",
    "expect_message",
    "expect_condition"
  )

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls(expect_condition_funs)
    na_expr <- xml_find_all_(xml_calls, na_xpath)
    bad_expr <- xml_find_first_(na_expr, "parent::expr")

    matched_function <- xp_call_name(bad_expr)
    replacement <- sub("^expect_", "expect_no_", matched_function)
    na_text <- xml_find_chr_(na_expr, "string(NUM_CONST)")
    is_named <- xml_find_lgl_(na_expr, "boolean(preceding-sibling::*[not(self::COMMENT)][1][self::EQ_SUB])")
    matched_arg <- ifelse(is_named, paste("regexp =", na_text), na_text)
    lint_message <- sprintf("%s(x) is better than %s(x, %s).", replacement, matched_function, matched_arg)

    xml_nodes_to_lints(bad_expr, source_expression, lint_message = lint_message, type = "warning")
  })
}
