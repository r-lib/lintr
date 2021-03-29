#' @describeIn linters Check that the line length of both comments and code is
#' less than length.
#' @export
line_length_linter <- function(length = 80L) {
  Linter(function(source_file) {

    # Only go over complete file
    if (is.null(source_file$file_lines)) return(list())

    line_lengths <- nchar(source_file$file_lines)
    long_lines <- which(line_lengths > length)

    lint_message <- sprintf("Lines should not be more than %d characters.", length)

    lapply(long_lines, function(long_line) {
      Lint(
        filename = source_file$filename,
        line_number = long_line,
        column_number = length + 1L,
        type = "style",
        message = lint_message,
        line = source_file$file_lines[long_line],
        ranges = list(c(1L, line_lengths[long_line]))
      )
    })
  })
}
