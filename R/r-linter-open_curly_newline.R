open_curly_newline_linter <- function(source_file) {
  lapply(which(source_file$parsed_content$token %in% "'{'"),
    function(id) {

      parsed <- source_file$parsed_content[id, ]

      line <- getSrcLines(source_file, parsed$line1, parsed$line1)

      if (parsed$col1 %!=% nchar(line)) {
        Lint(
          filename = source_file$filename,
          line_number = parsed$line1,
          column_number = parsed$col1 + 1L,
          type = "style",
          message = "Opening curly-braces should always be followed by a newline.",
          line = getSrcLines(source_file, parsed$line1, parsed$line1),
          ranges = list(c(parsed$col1 + 1L, nchar(line)))
          )
      }

    })
}
