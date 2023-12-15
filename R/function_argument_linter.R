#' Function argument linter
#'
#' @description
#' Check that arguments with defaults come last in all function declarations,
#' as per the tidyverse design guide.
#'
#' Changing the argument order can be a breaking change. An alternative to changing the argument order
#' is to instead set the default for such arguments to `NULL`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "function(y = 1, z = 2, x) {}",
#'   linters = function_argument_linter()
#' )
#'
#' lint(
#'   text = "function(x, y, z = 1, ..., w) {}",
#'   linters = function_argument_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "function(x, y = 1, z = 2) {}",
#'   linters = function_argument_linter()
#' )
#'
#' lint(
#'   text = "function(x, y, w, z = 1, ...) {}",
#'   linters = function_argument_linter()
#' )
#'
#' lint(
#'   text = "function(y = 1, z = 2, x = NULL) {}",
#'   linters = function_argument_linter()
#' )
#'
#' lint(
#'   text = "function(x, y, z = 1, ..., w = NULL) {}",
#'   linters = function_argument_linter()
#' )
#'
#' @evalRd rd_tags("function_argument_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://design.tidyverse.org/required-no-defaults.html>
#' @export
function_argument_linter <- function() {
  xpath <- "
  (//FUNCTION | //OP-LAMBDA)
    /following-sibling::EQ_FORMALS[1]
    /following-sibling::SYMBOL_FORMALS[
      text() != '...'
      and not(following-sibling::*[not(self::COMMENT)][1][self::EQ_FORMALS])
    ]
  "

  used_in_missing_xpath <- "
    text() = following-sibling::expr[last()]//expr[expr/SYMBOL_FUNCTION_CALL[text() = 'missing']]/expr[2]/SYMBOL/text()
  "

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)

    uses_missing <- xml_find_lgl(bad_expr, used_in_missing_xpath)

    missing_note <-
      ifelse(uses_missing, " Consider setting the default to NULL and using is.null() instead of using missing()", "")

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = paste0("Arguments without defaults should come before arguments with defaults.", missing_note),
      type = "style"
    )
  })
}
