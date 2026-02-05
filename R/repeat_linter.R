#' Repeat linter
#'
#' Check that `while (TRUE)` is not used for infinite loops. While this is valid
#' R code, using `repeat {}` is more explicit.
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
#' @evalRd rd_tags("repeat_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
repeat_linter <- function() {
  xpath <- "//WHILE[following-sibling::expr[1]/NUM_CONST[text() = 'TRUE']]/parent::expr"

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    lints <- xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      lints,
      source_expression = source_expression,
      lint_message = "Use 'repeat' instead of 'while (TRUE)' for infinite loops."
    )
  })
}
