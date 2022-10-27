#' Require usage of `lengths()` where possible
#'
#' [lengths()] is a function that was added to base R in version 3.2.0 to
#'   get the length of each element of a list. It is equivalent to
#'   `sapply(x, length)`, but faster and more readable.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "sapply(x, length)",
#'   linters = lengths_linter()
#' )
#'
#' lint(
#'   text = "vapply(x, length, integer(1L))",
#'   linters = lengths_linter()
#' )
#'
#' lint(
#'   text = "purrr::map_int(x, length)",
#'   linters = lengths_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "lengths(x)",
#'   linters = lengths_linter()
#' )
#'
#' @evalRd rd_tags("lengths_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
lengths_linter <- function() {
  loop_funs <- c("sapply", "vapply", "map_int", "map_dbl")
  xpath <- glue::glue("
  //SYMBOL_FUNCTION_CALL[ {xp_text_in_table(loop_funs)} ]
    /parent::expr
    /parent::expr[expr/SYMBOL[text() = 'length']]
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = "Use lengths() to find the length of each element in a list.",
      type = "warning"
    )
  })
}
