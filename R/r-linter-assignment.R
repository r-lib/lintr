assignment_linter <- function(source_file) {
  lapply(which(source_file$parsed_content$token == "EQ_ASSIGN"),
    function(id) {
      parsed <- source_file$parsed_content[id, ]
      Lint(
        filename = source_file$filename,
        line_number = parsed$line1,
        column_number = parsed$col1,
        type = "style",
        message = "Use <-, not =, for assignment.",
        line = getSrcLines(source_file, parsed$line1, parsed$line1)
        )
    })
}
