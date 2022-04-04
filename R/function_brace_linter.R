#' Require multi-line functions to use braces
#'
#' This linter catches function definitions spanning multiple lines of code
#'   that aren't wrapped in braces
#'
#' @evalRd rd_tags("function_brace_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#indenting>
#' @export
function_brace_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$xml_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    bad_expr_xpath <- "//expr[FUNCTION and @line1 != @line2 and not(expr[OP-LEFT-BRACE])]"
    bad_expr <- xml2::xml_find_all(xml, bad_expr_xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = "Any function spanning multiple lines must use curly braces.",
      type = "style"
    ))
  })
}
