spaces_right_parentheses <- function(source_file) {
  lapply(which(source_file$parsed_content$token %in% "')'"),
    function(id) {

      parsed <- source_file$parsed_content[id, ]
      line <- getSrcLines(source_file, parsed$line1, parsed$line1)

      after_operator <- substr(line, parsed$col1 + 1L, parsed$col1 + 1L)

      non_space_after <- re_matches(after_operator, rex(non_space))

      if(non_space_after) {
        Lint(
          filename = source_file$filename,
          line_number = parsed$line1,
          column_number = parsed$col1,
          type = "style",
          message = "Always place a space after right parenthesis.",
          line = getSrcLines(source_file, parsed$line1, parsed$line1)
          )
      }
    })
}

