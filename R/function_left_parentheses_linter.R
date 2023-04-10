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
  xpath_fmt <- "//FUNCTION[ {xpath} ] | //SYMBOL_FUNCTION_CALL/parent::expr[ {xpath} ]"
  bad_line_cond <- "@line1 != following-sibling::OP-LEFT-PAREN/@line1"
  bad_col_cond <- xp_and(
    "@line1 = following-sibling::OP-LEFT-PAREN/@line1",
    "@col2 != following-sibling::OP-LEFT-PAREN/@col1 - 1"
  )
  bad_line_xpath <- glue::glue(xpath_fmt, xpath = bad_line_cond)
  bad_col_xpath <- glue::glue(xpath_fmt, xpath = bad_col_cond)

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_line_exprs <- xml2::xml_find_all(xml, bad_line_xpath)
    bad_line_lints <- xml_nodes_to_lints(
      bad_line_exprs,
      source_expression = source_expression,
      lint_message = "Left parenthesis should be on the same line as the function's symbol.",
    )

    bad_col_exprs <- xml2::xml_find_all(xml, bad_col_xpath)
    bad_col_lints <- xml_nodes_to_lints(
      bad_col_exprs,
      source_expression = source_expression,
      lint_message = "Remove spaces before the left parenthesis in a function call.",
      range_start_xpath = "number(./@col2 + 1)", # start after function / fun
      range_end_xpath = "number(./following-sibling::OP-LEFT-PAREN/@col1 - 1)" # end before (
    )
    c(bad_line_lints, bad_col_lints)
  })
}
