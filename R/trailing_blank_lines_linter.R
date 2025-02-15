#' Trailing blank lines linter
#'
#' Check that there are no trailing blank lines in source code.
#'
#' @examples
#' # will produce lints
#' f <- tempfile()
#' cat("x <- 1\n\n", file = f)
#' writeLines(readChar(f, file.size(f)))
#' lint(
#'   filename = f,
#'   linters = trailing_blank_lines_linter()
#' )
#' unlink(f)
#'
#' # okay
#' cat("x <- 1\n", file = f)
#' writeLines(readChar(f, file.size(f)))
#' lint(
#'   filename = f,
#'   linters = trailing_blank_lines_linter()
#' )
#' unlink(f)
#'
#' @evalRd rd_tags("trailing_blank_lines_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
trailing_blank_lines_linter <- function() {
  Linter(function(source_expression) {
    blanks <- re_matches(
      source_expression$file_lines,
      rex(start, any_spaces, end)
    )

    line_number <- length(source_expression$file_lines)
    lints <- list()
    while (line_number > 0L && (is.na(blanks[[line_number]]) || isTRUE(blanks[[line_number]]))) {
      if (!is.na(blanks[[line_number]])) {
        lints[[length(lints) + 1L]] <- Lint(
          filename = source_expression$filename,
          line_number = line_number,
          column_number = 1L,
          type = "style",
          message = "Remove trailing blank lines.",
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
        column_number = (nchar(last_line) %||% 0L) + 1L,
        type = "style",
        message = "Add a terminal newline.",
        line = last_line
      )
    }

    lints
  })
}
