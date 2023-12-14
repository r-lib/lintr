#' TODO comment linter
#'
#' Check that the source contains no TODO comments (case-insensitive).
#'
#' @param todo Vector of strings that identify TODO comments.
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
  Linter(function(source_expression) {
    tokens <- with_id(source_expression, ids_with_token(source_expression, "COMMENT"))
    are_todo <- re_matches(tokens[["text"]], todo_comment_regex, ignore.case = TRUE)
    tokens <- tokens[are_todo, ]
    lapply(
      split(tokens, seq_len(nrow(tokens))),
      function(token) {
        Lint(
          filename = source_expression[["filename"]],
          line_number = token[["line1"]],
          column_number = token[["col1"]],
          type = "style",
          message = "Remove TODO comments.",
          line = source_expression[["lines"]][[as.character(token[["line1"]])]],
          ranges = list(c(token[["col1"]], token[["col2"]]))
        )
      }
    )
  })
}
