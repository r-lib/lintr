#' Function argument linter
#'
#' Check that arguments with defaults come last in all function declarations.
#'
#' @evalRd rd_tags("function_argument_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr.
#' @export
function_argument_linter <- function() {
  xpath <- "//FUNCTION/following-sibling::EQ_FORMALS[1]/
              following-sibling::SYMBOL_FORMALS[not(following-sibling::*[1][self::EQ_FORMALS])]"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = "Arguments without defaults should come before arguments with defaults.",
      type = "style"
    )
  })
}
