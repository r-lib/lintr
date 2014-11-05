#' @describeIn linters check there are no trailing whitespace characters.
#' @export
trailing_whitespace_linter <- function(source_file) {
  res <- re_matches(source_file$content,
    rex(capture(name = "space", some_of(" ", regex("\\t"))), or(newline, end)),
    global = TRUE,
    locations = TRUE)[[1]]

  if (!is.na(res$space.start[1])) {
    lapply(seq_len(NROW(res)),
      function(itr) {
        line_number <- source_file$find_line(res$space.start[itr])
        column_start <- source_file$find_column(res$space.start[itr])
        column_end <- source_file$find_column(res$space.end[itr])

        Lint(
          filename = source_file$filename,
          line_number = line_number,
          column_number = column_start,
          type = "style",
          message = "Trailing whitespace is superfluous.",
          line = getSrcLines(source_file, line_number, line_number),
          ranges = list(c(column_start, column_end))
          )
    })
  }

}
