#' Library call linter
#'
#' Force library calls to all be at the top of the script.
#'
#' @examples
#'  # will produce lints
#'  lint(
#'    text = c("library(dplyr)", "print('test')", "library(tidyr)"),
#'    linters = library_call_linter()
#'  )
#'
#'  # okay
#'  lint(
#'    text = c("library(dplyr)", "print('test')"),
#'    linters = library_call_linter()
#'  )
#'
#'  lint(
#'    text = c("# comment", "library(dplyr)"),
#'    linters = library_call_linter()
#'  )
#'
#' @evalRd rd_tags("library_call_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
library_call_linter <- function() {

  xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'library'][last()]
  //preceding::expr
  /SYMBOL_FUNCTION_CALL[text() != 'library'][last()]
  "
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    if (length(bad_expr) == 0L) {
      return(list())
    }

    bad_expr_library<- xml2::xml_find_all(xml, "//SYMBOL_FUNCTION_CALL[text() = 'library'][last()]")

    xml_nodes_to_lints(
      bad_expr_library,
      source_expression = source_expression,
      lint_message = "Move all library calls to the top of the script.",
      type = "warning"
    )
  })
}
