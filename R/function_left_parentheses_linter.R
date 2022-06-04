#' Function left parentheses linter
#'
#' Check that all left parentheses in a function call do not have spaces before them.
#'
#' @evalRd rd_tags("function_left_parentheses_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#parentheses>
#' @export
function_left_parentheses_linter <- function() { # nolint: object_length.
  xpath <- "
    //FUNCTION[@col2 != following-sibling::OP-LEFT-PAREN/@col1 - 1] |
    //expr[1][SYMBOL_FUNCTION_CALL and @col2 != following-sibling::OP-LEFT-PAREN/@col1 - 1]
  "

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
      range_end_xpath = "number(./following-sibling::OP-LEFT-PAREN/@col1 - 1)" # end before (
    )
  })
}
