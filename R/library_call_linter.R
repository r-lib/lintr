#' Library call linter
#'
#' Force library calls to all be at the top of the script.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "
#'     library(dplyr)
#'     print('test')
#'     library(tidyr)
#'   ",
#'   linters = library_call_linter()
#' )
#'
#' lint(
#'   text = "
#'     library(dplyr)
#'     print('test')
#'     library(tidyr)
#'     library(purrr)
#'   ",
#'   linters = library_call_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "
#'     library(dplyr)
#'     print('test')
#'   ",
#'   linters = library_call_linter()
#' )
#'
#' lint(
#'   text = "
#'     # comment
#'     library(dplyr)
#'   ",
#'   linters = library_call_linter()
#' )
#'
#' @evalRd rd_tags("library_call_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
library_call_linter <- function() {
  attach_call <- "text() = 'library' or text() = 'require'"
  xpath <- glue("
    //SYMBOL_FUNCTION_CALL[{ attach_call }][last()]
      /preceding::expr
      /SYMBOL_FUNCTION_CALL[not({ attach_call } or starts-with(text(), 'suppres'))][last()]
      /following::expr[SYMBOL_FUNCTION_CALL[{ attach_call }]]
      /parent::expr
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)

    call_name <- xp_call_name(bad_expr)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = sprintf("Move all %s calls to the top of the script.", call_name),
      type = "warning"
    )
  })
}
