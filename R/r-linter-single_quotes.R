single_quotes_linter <- function(source_file) {
  lapply(which(source_file$parsed_content$token == "STR_CONST"),
    function(id) {
      parsed <- source_file$parsed_content[id, ]
      if(re_matches(parsed$text, rex(start, "'", anything, "'", end))) {
        Lint(
          filename = source_file$filename,
          line_number = parsed$line1,
          column_number = parsed$col1,
          type = "style",
          message = "Only use double-quotes.",
          line = getSrcLines(source_file, parsed$line1, parsed$line1),
          ranges = list(c(parsed$col1, parsed$col2))
          )
      }
    })
}
