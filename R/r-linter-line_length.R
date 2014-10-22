line_length_linter <- function(width) {
  function(source_file) {

  lapply(which(source_file$lengths > width),
    function(line_number){
      col_start <- 1
      col_end <- source_file$lengths[line_number]

      Lint(
        filename = source_file$filename,
        line_number = line_number,
        column_number = col_start,
        type = "style",
        message = sprintf("lines should not be more than %d characters", width),
        line = getSrcLines(source_file, line_number, line_number),
        ranges = list(c(col_start, col_end))
        )
    })
  }
}
