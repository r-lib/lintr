assignment_linter <- function(source_file) {
  lapply(which(source_file$parsed_content$token == "EQ_ASSIGN"),
    function(id) {
      parsed <- source_file$parsed_content[id, ]
      lint(
        filename = source_file$filename,
        line_number = parsed$line1,
        column_number = parsed$col1,
        type = "style",
        message = "Use <-, not =, for assignment.",
        line = getSrcLines(source_file, parsed$line1, parsed$line1)
        )
    })
}

single_quotes_linter <- function(source_file) {
  lapply(which(source_file$parsed_content$token == "STR_CONST"),
    function(id) {
      line_num <- source_file$parsed_content$line1[id]
      col_num_1 <- source_file$parsed_content$col1[id]
      col_num_2 <- source_file$parsed_content$col2[id]
      if(re_matches(source_file$parsed_content$text[id], rex(start, "'", anything, "'", end))) {
        lint(
          filename = source_file$filename,
          line_number = line_num,
          column_number = col_num_1,
          type = "style",
          message = "Only use Double-Quotes.",
          line = getSrcLines(source_file, line_num, line_num),
          ranges = list(c(col_num_1, col_num_2))
          )
      }
    })
}

default_linters <- c(

  assignment_linter,
  single_quotes_linter,

  NULL
)
