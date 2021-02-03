#' @describeIn linters check the line length of both comments and code is less
#' than length.
#' @export
line_length_linter <- function(length = 80L) {
  function(source_file) {

    # Only go over complete file
    if (is.null(source_file$file_lines)) return(list())

    oversized_lines <- which(nchar(source_file$file_lines) > length)

    lapply(oversized_lines,
      function(line_number) {

        col_start <- 1
        line <- source_file$file_lines[line_number]
        col_end <- unname(nchar(line))

        Lint(
          filename = source_file$filename,
          line_number = line_number,
          column_number = col_start,
          type = "style",
          message = sprintf("Lines should not be more than %d characters.", length),
          line = line,
          ranges = list(c(col_start, col_end)),
          linter = "line_length_linter"
          )
      })
  }
}
