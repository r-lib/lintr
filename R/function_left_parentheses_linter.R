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
  pos_xpath <- xp_or(
    "@line1 != following-sibling::OP-LEFT-PAREN/@line1",
    "@col2 != following-sibling::OP-LEFT-PAREN/@col1 - 1"
  )
  xpath <- glue::glue("//FUNCTION[ {pos_xpath}] | //SYMBOL_FUNCTION_CALL/parent::expr[ {pos_xpath} ]")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content
    bad_exprs <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_exprs,
      source_expression = source_expression,
      lint_message = "Remove spaces before the left parenthesis in a function call.",
      range_start_xpath = "number(./@col2 + 1)", # start after function / fun
      # if '(' is on the same line, end before '(', otherwise, just set range[2]=range[1].
      # this is a janky XPath 1.0 implementation for if(same line, "before (", "range[1]")
      #   by converting if(COND, x, y) to 1[COND] * x + (1 - 1[COND]) * y for 1[.] an indicator function
      range_end_xpath = "
        number(./@line1  = ./following-sibling::OP-LEFT-PAREN/@line1) *
          number(./following-sibling::OP-LEFT-PAREN/@col1 - 1) +
        number(./@line1 != ./following-sibling::OP-LEFT-PAREN/@line1) *
          number(./@col2 + 1)
      "
    )
  })
}
