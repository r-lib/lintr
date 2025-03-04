#' TODO comment linter
#'
#' Check that the source contains no TODO comments (case-insensitive).
#'
#' @param todo Vector of case-insensitive strings that identify TODO comments.
#' @param except_regex Vector of case-sensitive regular expressions that identify
#'    _valid_ TODO comments.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x + y # TOODOO",
#'   linters = todo_comment_linter(todo = "toodoo")
#' )
#'
#' lint(
#'   text = "pi <- 1.0 # FIIXMEE",
#'   linters = todo_comment_linter(todo = "fiixmee")
#' )
#'
#' lint(
#'   text = "x <- TRUE # TOODOO(#1234): Fix this hack.",
#'   linters = todo_comment_linter()
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
#' lint(
#'   text = "x <- TRUE # TODO(#1234): Fix this hack.",
#'   linters = todo_comment_linter(except_regex = "TODO\\(#[0-9]+\\):")
#' )
#'
#' @evalRd rd_tags("todo_comment_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
todo_comment_linter <- function(todo = c("todo", "fixme"), except_regex = NULL) {
  todo_comment_regex <- rex(one_or_more("#"), any_spaces, or(todo))
  valid_todo_regex <-
    if (!is.null(except_regex)) paste0("#+", rex::shortcuts$any_spaces, "(?:", paste(except_regex, collapse = "|"), ")")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    comment_expr <- xml_find_all(xml, "//COMMENT")
    comment_text <- xml_text(comment_expr)
    invalid_todo <- re_matches_logical(comment_text, todo_comment_regex, ignore.case = TRUE)
    if (!is.null(valid_todo_regex)) {
      invalid_todo <- invalid_todo & !re_matches_logical(comment_text, valid_todo_regex)
    }

    xml_nodes_to_lints(
      comment_expr[invalid_todo],
      source_expression = source_expression,
      lint_message = "Remove TODO comments.",
      type = "style"
    )
  })
}
