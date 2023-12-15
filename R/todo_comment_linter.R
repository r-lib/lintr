#' TODO comment linter
#'
#' Check that the source contains no TODO comments (case-insensitive).
#'
#' @param todo Vector of case-insensitive strings that identify TODO comments.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x + y # TODO",
#'   linters = todo_comment_linter()
#' )
#'
#' lint(
#'   text = "pi <- 1.0 # FIXME",
#'   linters = todo_comment_linter()
#' )
#'
#' lint(
#'   text = "x <- TRUE # hack",
#'   linters = todo_comment_linter(todo = c("todo", "fixme", "hack"))
#' )
#'
#' # okay
#' lint(
#'   text = "x + y # my informative comment",
#'   linters = todo_comment_linter()
#' )
#'
#' lint(
#'   text = "pi <- 3.14",
#'   linters = todo_comment_linter()
#' )
#'
#' lint(
#'   text = "x <- TRUE",
#'   linters = todo_comment_linter()
#' )
#'
#' @evalRd rd_tags("todo_comment_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
todo_comment_linter <- function(todo = c("todo", "fixme")) {
  todo_comment_regex <- rex(one_or_more("#"), any_spaces, or(todo))

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    comment_expr <- xml_find_all(xml, "//COMMENT")
    are_todo <- re_matches(xml_text(comment_expr), todo_comment_regex, ignore.case = TRUE)

    xml_nodes_to_lints(
      comment_expr[are_todo],
      source_expression = source_expression,
      lint_message = "Remove TODO comments.",
      type = "style"
    )
  })
}
