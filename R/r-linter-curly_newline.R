curly_own_line_linter <- function(source_file) {
  lapply(which(source_file$parsed_content$token %in% "'{'"),
    function(id) {

      parsed <- source_file$parsed_content[id, ]

      line <- getSrcLines(source_file, parsed$line1, parsed$line1)

      before <- substr(line, 1L, parsed$col1)

      if (parsed$col1 %==% 1L || re_matches(before, rex(start, spaces, "{"))) {
        Lint(
          filename = source_file$filename,
          line_number = parsed$line1,
          column_number = parsed$col1,
          type = "style",
          message = "Opening curly-braces should never be on their own line.",
          line = getSrcLines(source_file, parsed$line1, parsed$line1),
          ranges = list(c(0L, parsed$col1))
          )
      }

    })
}
