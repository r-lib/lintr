#' Function argument linter
#'
#' Check that arguments with defaults come last in all function declarations, as per the tidyverse design guide.
#'
#' @evalRd rd_tags("function_argument_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://design.tidyverse.org/args-data-details.html>
#' @export
function_argument_linter <- function() {
  xpath <- paste(glue::glue(
    "//{c('FUNCTION', 'OP-LAMBDA')}/following-sibling::EQ_FORMALS[1]/
              following-sibling::SYMBOL_FORMALS[not(following-sibling::*[not(self::COMMENT)][1][self::EQ_FORMALS])]"
  ), collapse = " | ")

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
