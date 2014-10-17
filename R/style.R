assignment_linter <- function(source_file) {
  lapply(which(source_file$parsed_content$token == "EQ_ASSIGN"),
    function(id) {
      line_num <- source_file$parsed_content$line1[id]
      col_num <- source_file$parsed_content$col1[id]
      lint(
        filename = source_file$filename,
        line_number = line_num,
        column_number = col_num,
        type = "style",
        message = "Use <-, not =, for assignment.",
        line = getSrcLines(source_file, line_num, line_num)
        )
    })
}
