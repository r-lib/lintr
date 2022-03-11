#' Assignment spaces linter
#'
#' Check that assignments only have one space before and after.
#'
#' @evalRd rd_tags("assignment_spaces_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
assignment_spaces_linter <- function() {
  Linter(function(source_file) {
    lapply(
      c(
        ids_with_token(source_file, "LEFT_ASSIGN"),
        ids_with_token(source_file, "EQ_ASSIGN")
      ),
      function(id) {
        parsed <- with_id(source_file, id)
        match <- re_matches(
          substr(
            x = source_file$lines[parsed$line1],
            start = parsed$col1 - 2,
            stop = parsed$col1 + nchar(parsed$text) + 1
          ),
          rex(at_least(space, 2))
        )
        if (match) {
          Lint(
            filename = source_file$filename,
            line_number = parsed$line1,
            column_number = parsed$col1,
            type = "style",
            message = "Assignments should only have one space before and after the operator.",
            line = source_file$lines[parsed$line1]
          )
        }
      }
    )
  })
}
