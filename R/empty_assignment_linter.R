#' Block assignment of `{}`
#'
#' Assignment of `{}` is the same as assignment of `NULL`; use the latter
#'   for clarity. Closely related: [unnecessary_concatenation_linter()].
#'
#' @examples
#' # will produce lints
#' code_lines <- "x <- {\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = empty_assignment_linter()
#' )
#'
#' # okay
#' code_lines <- "x <- {\n3L + 4L\n}"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = empty_assignment_linter()
#' )
#'
#' lint(
#'   text = "x <- NULL",
#'   linters = empty_assignment_linter()
#' )
#'
#' @evalRd rd_tags("empty_assignment_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
empty_assignment_linter <- function() {
  # for some reason, the parent in the `=` case is <equal_assign>, not <expr>, hence parent::expr
  xpath <- "
  //OP-LEFT-BRACE[following-sibling::*[1][self::OP-RIGHT-BRACE]]
    /parent::expr[
      preceding-sibling::LEFT_ASSIGN
      or preceding-sibling::EQ_ASSIGN
      or following-sibling::RIGHT_ASSIGN
    ]
    /parent::*
  "

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message =
        "Assign NULL explicitly or, whenever possible, allocate the empty object with the right type and size.",
      type = "warning"
    )
  })
}
