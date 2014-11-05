#' @describeIn linters check there is a newline at the end of the file.
#' @export
eof_newline_linter <- function(source_file) {
  res <- re_matches(source_file$content,
    rex(regex("\\Z"), newline),
    locations = TRUE)

  if (is.na(res$start)) {
    line_number <- source_file$find_line(nchar(source_file$content))
    column_start <- source_file$find_column(nchar(source_file$content))

      Lint(
        filename = source_file$filename,
        line_number = line_number,
        column_number = column_start,
        type = "style",
        message = "Last line should end with a newline.",
        line = getSrcLines(source_file, line_number, line_number)
        )
  }

}
