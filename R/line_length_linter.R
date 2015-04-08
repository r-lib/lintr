#' @describeIn linters check the line length of both comments and code is less
#' than length.
#' @export
line_length_linter <- function(length) {
  function(source_file) {

    lapply(names(source_file$lines)[vapply(source_file$lines, nchar, integer(1)) > length],
      function(line_number) {
        col_start <- 1
        line <- source_file$lines[as.character(line_number)]
        col_end <- unname(nchar(line))

        Lint(
          filename = source_file$filename,
          line_number = line_number,
          column_number = col_start,
          type = "style",
          message = sprintf("lines should not be more than %d characters.", length),
          line = line,
          ranges = list(c(col_start, col_end)),
          linter = "line_length_linter"
          )
      })
  }
}
