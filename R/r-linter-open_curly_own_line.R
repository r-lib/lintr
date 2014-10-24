open_curly_own_line_linter <- function(source_file) {
  lapply(which(source_file$parsed_content$token %in% "'{'"),
    function(id) {

      parsed <- source_file$parsed_content[id, ]

      tokens_on_line <- source_file$parsed_content[
        source_file$parsed_content$line1 == parsed$line1 &
        source_file$parsed_content$col1 <= parsed$col1,
        "token"
        ]

      # the only tokens should be the { and the start of the expression.
      has_no_other_expr <- length(tokens_on_line) %==% 2L

      # if the closing curly has an expression on the same line, and there is
      # not also an else
      if (has_no_other_expr) {
        Lint(
          filename = source_file$filename,
          line_number = parsed$line1,
          column_number = parsed$col1,
          type = "style",
          message = "Opening curly-braces should always be on their own line.",
          line = getSrcLines(source_file, parsed$line1, parsed$line1),
          ranges = list(c(0L, parsed$col1))
          )
      }

    })
}

