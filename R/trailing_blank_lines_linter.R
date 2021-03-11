#' @describeIn linters  Check that there are no trailing blank lines in source
#' files.
#' @export
trailing_blank_lines_linter <- function() {
  Linter(function(source_file) {
    blanks <- re_matches(source_file$file_lines,
                         rex(start, any_spaces, end))

    line_number <- length(source_file$file_lines)
    lints <- list()
    while (line_number > 0L && (is.na(blanks[[line_number]]) || isTRUE(blanks[[line_number]]))) {
      if (!is.na(blanks[[line_number]])) {
        lints[[length(lints) + 1L]] <- Lint(
          filename = source_file$filename,
          line_number = line_number,
          column_number = 1,
          type = "style",
          message = "Trailing blank lines are superfluous.",
          line = source_file$file_lines[[line_number]]
        )
      }
      line_number <- line_number - 1L
    }
    if (identical(source_file$terminal_newline, FALSE)) { # could use isFALSE, but needs backports
      last_line <- tail(source_file$file_lines, 1L)

      lints[[length(lints) + 1L]] <- Lint(
        filename = source_file$filename,
        line_number = length(source_file$file_lines),
        column_number = nchar(last_line) + 1L,
        type = "style",
        message = "Missing terminal newline.",
        line = last_line
      )
    }
    lints
  })
}
