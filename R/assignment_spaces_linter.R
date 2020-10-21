#' @describeIn linters checks that assignments only have one space before and after
#' @export
assignment_spaces <- function(source_file) {
  lapply(
    c(
      ids_with_token(source_file, "LEFT_ASSIGN"),
      ids_with_token(source_file, "EQ_ASSIGN")
    ),
    function(id) {
      parsed <- with_id(source_file, id)
      match <- re_matches(
        source_file$lines[as.character(parsed$line1)],
        rex(list(one_or_more(space), space, "<-" %or% "=") %or% list("<-" %or% "=", space, one_or_more(space)))
      )
      if (match) {
        Lint(
          filename = source_file$filename,
          line_number = parsed$line1,
          column_number = parsed$col1,
          type = "style",
          message = "Assignments should only have one space before and after the operator.",
          line = source_file$lines[as.character(parsed$line1)],
          linter = "assignment_spaces"
        )
      }
    }
  )
}
