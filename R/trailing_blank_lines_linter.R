#' @describeIn linters check there are no trailing blank lines.
#' @export
trailing_blank_lines_linter <- function(source_file) {
  blanks <- re_matches(source_file$file_lines,
    rex(start, any_spaces, end))

  line_number <- length(source_file$file_lines)
  lints <- list()
  while (line_number > 0L && (is.na(blanks[[line_number]]) || isTRUE(blanks[[line_number]]))) {
    if (!is.na(blanks[[line_number]])) {
      lints[[length(lints) + 1L]] <-
        Lint(
          filename = source_file$filename,
          line_number = line_number,
          column_number = 1,
          type = "style",
          message = "Trailing blank lines are superfluous.",
          line = source_file$file_lines[[line_number]],
          linter = "trailing_blank_lines_linter"
          )
    }
    line_number <- line_number - 1L
  }
  lints
}
