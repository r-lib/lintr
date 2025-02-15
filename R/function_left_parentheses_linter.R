#' Function left parentheses linter
#'
#' Check that all left parentheses in a function call do not have spaces before them
#'  (e.g. `mean  (1:3)`). Although this is syntactically valid, it makes the code
#'  difficult to read.
#'
#' Exceptions are made for control flow functions (`if`, `for`, etc.).
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "mean (x)",
#'   linters = function_left_parentheses_linter()
#' )
#'
#' lint(
#'   text = "stats::sd(c (x, y, z))",
#'   linters = function_left_parentheses_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "mean(x)",
#'   linters = function_left_parentheses_linter()
#' )
#'
#' lint(
#'   text = "stats::sd(c(x, y, z))",
#'   linters = function_left_parentheses_linter()
#' )
#'
#' lint(
#'   text = "foo <- function(x) (x + 1)",
#'   linters = function_left_parentheses_linter()
#' )
#'
#' @evalRd rd_tags("function_left_parentheses_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#parentheses>
#' - [spaces_left_parentheses_linter()]
#' @export
function_left_parentheses_linter <- function() { # nolint: object_length.
  # NB: attach to SYMBOL_FUNCTION_CALL instead of SYMBOL_FUNCTION_CALL/parent::expr because
  #   the latter might be associated with a different line, e.g. in the case of a
  #   complicated call to an "extracted" function (see #1963). This mistake was made earlier
  #   because it allows the xpath to be the same for both FUNCTION and SYMBOL_FUNCTION_CALL.
  #   Further, write 4 separate XPaths because the 'range_end_xpath' differs for these two nodes.
  bad_line_fun_xpath <- "(//FUNCTION | //OP-LAMBDA)[@line1 != following-sibling::OP-LEFT-PAREN/@line1]"
  bad_line_call_xpath <- "//SYMBOL_FUNCTION_CALL[@line1 != parent::expr/following-sibling::OP-LEFT-PAREN/@line1]"
  bad_col_fun_xpath <- "(//FUNCTION | //OP-LAMBDA)[
    @line1 = following-sibling::OP-LEFT-PAREN/@line1
    and @col2 != following-sibling::OP-LEFT-PAREN/@col1 - 1
  ]"
  bad_col_call_xpath <- "//SYMBOL_FUNCTION_CALL[
    @line1 = parent::expr/following-sibling::OP-LEFT-PAREN/@line1
    and @col2 != parent::expr/following-sibling::OP-LEFT-PAREN/@col1 - 1
  ]"

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_line_fun_exprs <- xml_find_all(xml, bad_line_fun_xpath)
    bad_line_fun_lints <- xml_nodes_to_lints(
      bad_line_fun_exprs,
      source_expression = source_expression,
      lint_message = "Left parenthesis should be on the same line as the 'function' symbol."
    )

    bad_line_call_exprs <- xml_find_all(xml, bad_line_call_xpath)
    bad_line_call_lints <- xml_nodes_to_lints(
      bad_line_call_exprs,
      source_expression = source_expression,
      lint_message = "Left parenthesis should be on the same line as the function's symbol."
    )

    bad_col_fun_exprs <- xml_find_all(xml, bad_col_fun_xpath)
    bad_col_fun_lints <- xml_nodes_to_lints(
      bad_col_fun_exprs,
      source_expression = source_expression,
      lint_message = "Remove spaces before the left parenthesis in a function definition.",
      range_start_xpath = "number(./@col2 + 1)", # start after function
      range_end_xpath = "number(./following-sibling::OP-LEFT-PAREN/@col1 - 1)" # end before (
    )

    bad_col_call_exprs <- xml_find_all(xml, bad_col_call_xpath)
    bad_col_call_lints <- xml_nodes_to_lints(
      bad_col_call_exprs,
      source_expression = source_expression,
      lint_message = "Remove spaces before the left parenthesis in a function call.",
      range_start_xpath = "number(./@col2 + 1)", # start after call name
      range_end_xpath = "number(./parent::expr/following-sibling::OP-LEFT-PAREN/@col1 - 1)" # end before (
    )

    c(bad_line_fun_lints, bad_line_call_lints, bad_col_fun_lints, bad_col_call_lints)
  })
}
