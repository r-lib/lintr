#' Trailing blank lines linter
#'
#' Check that there are no trailing blank lines in source code.
#'
#' @evalRd rd_tags("trailing_blank_lines_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
trailing_blank_lines_linter <- function() {
  Linter(function(source_expression) {
    blanks <- re_matches(source_expression$file_lines,
                         rex(start, any_spaces, end))

    line_number <- length(source_expression$file_lines)
    lints <- list()
    while (line_number > 0L && (is.na(blanks[[line_number]]) || isTRUE(blanks[[line_number]]))) {
      if (!is.na(blanks[[line_number]])) {
        lints[[length(lints) + 1L]] <- Lint(
          filename = source_expression$filename,
          line_number = line_number,
          column_number = 1L,
          type = "style",
          message = "Trailing blank lines are superfluous.",
          line = source_expression$file_lines[[line_number]]
        )
      }
      line_number <- line_number - 1L
    }
    if (identical(source_expression$terminal_newline, FALSE)) { # could use isFALSE, but needs backports
      last_line <- tail(source_expression$file_lines, 1L)

      lints[[length(lints) + 1L]] <- Lint(
        filename = source_expression$filename,
        line_number = length(source_expression$file_lines),
        column_number = nchar(last_line) %||% 0L + 1L,
        type = "style",
        message = "Missing terminal newline.",
        line = last_line
      )
    }
    lints
  })
}
