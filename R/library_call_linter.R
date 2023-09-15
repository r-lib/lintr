#' Library call linter
#'
#' Force library calls to all be at the top of the script.
#'
#' @param allow_preamble Logical, default `TRUE`. If `FALSE`,
#'   no code is allowed to precede the first `library()` call,
#'   otherwise some setup code is allowed, but all `library()`
#'   calls must follow consecutively after the first one.
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
library_call_linter <- function(allow_preamble = TRUE) {
  attach_call <- "text() = 'library' or text() = 'require'"
  unsuppressed_call <- glue("not( {attach_call} or starts-with(text(), 'suppress'))")
  if (allow_preamble) {
    unsuppressed_call <- xp_and(
      unsuppressed_call,
      glue("@line1 > //SYMBOL_FUNCTION_CALL[{ attach_call }][1]/@line1")
    )
  }
  xpath <- glue("
    //SYMBOL_FUNCTION_CALL[{ attach_call }][last()]
      /preceding::expr
      /SYMBOL_FUNCTION_CALL[{ unsuppressed_call }][last()]
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
