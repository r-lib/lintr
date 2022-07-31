#' Trailing assignment linter
#'
#' Check that there are no assignment operators at the end of source lines.
#'
#' @param allow_comments Suppress lints for commented out lines
#'
#' @evalRd rd_tags("trailing_whitespace_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
trailing_assignment_linter <- function(allow_comments = FALSE) {
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    res <- re_matches(
      source_expression$file_lines,
      rex(capture(or("<<-", "<-", "=", "->", "-->")), any_spaces, end)
    )
    bad_lines <- which(!vapply(res[, 1L], is.na, logical(1L), USE.NAMES = FALSE))

    if (allow_comments) {
      comment_lines <- grep("#", source_expression$file_lines, fixed = TRUE)
      bad_lines <- setdiff(bad_lines, comment_lines)
    }

    lapply(
      seq_along(bad_lines),
      function(x) {
        line <- bad_lines[x]
        Lint(
          filename = source_expression$filename,
          line_number = line,
          column_number = 1L,
          type = "style",
          message = paste0(
            "Assignment `", res[line, 1L], "` should not be trailing at end of line"
          ),
          line = source_expression$file_lines[[line]]
        )
      }
    )
  })
}
