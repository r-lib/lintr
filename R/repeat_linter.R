#' Repeat linter
#'
#' Check that `while` is not used for infinite loops.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "while (TRUE) { }",
#'   linters = repeat_linter()
#' )
#'
#'
#' # okay
#' lint(
#'   text = "repeat { }",
#'   linters = repeat_linter()
#' )
#'
#'
#' @evalRd rd_tags("repeat_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
repeat_linter <- function() {
  xpath <- "
  //WHILE[following-sibling::*[1][self::OP-LEFT-PAREN] and
  following-sibling::*[2][self::expr[NUM_CONST[text()='TRUE'] and count(*)=1]]
  and following-sibling::*[3][self::OP-RIGHT-PAREN]]"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }
    xml <- source_expression$xml_parsed_content


    lints <- xml_nodes_to_lints(
      xml_find_all(xml, xpath),
      source_expression = source_expression,
      lint_message = "'while (TRUE)' is not recommended for infinite loops. Use 'repeat' instead.",
      range_start_xpath = "number(./@col1)",
      range_end_xpath = "number(./following-sibling::*[3]/@col2)"
    )

    lints
  })
}
