#' @describeIn linters check there are no trailing whitespace characters.
#' @export
trailing_whitespace_linter <- function(source_file) {
  res <- re_matches(source_file$content,
    rex(capture(name = "space", spaces), or(newline, end)),
    locations = TRUE)

  if (!is.na(res$space.start)) {
    line_number <- source_file$find_line(res$space.start)
    column_start <- source_file$find_column(res$space.start)
    column_end <- source_file$find_column(res$space.end)

      Lint(
        filename = source_file$filename,
        line_number = line_number,
        column_number = column_start,
        type = "style",
        message = "Trailing whitespace is superfluous.",
        line = getSrcLines(source_file, line_number, line_number),
        ranges = list(c(column_start, column_end))
        )
  }

}
