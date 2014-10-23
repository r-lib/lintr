spaces_inside_linter <- function(source_file) {
  matches <- c(
    "'('",
    "')'",
    "'['",
    "']'")

  lapply(which(source_file$parsed_content$token %in% matches),
    function(id) {

      parsed <- source_file$parsed_content[id, ]

      line <- getSrcLines(source_file, parsed$line1, parsed$line1)

      one_before_operator <- substr(line, parsed$col1 - 1L, parsed$col1 - 1L)
      two_before_operator <- substr(line, parsed$col1 - 2L, parsed$col1 - 2L)
      one_after_operator <- substr(line, parsed$col1 + 1L, parsed$col1 + 1L)

      non_space_before <- re_matches(one_before_operator, rex(space))
      non_space_after <- re_matches(one_after_operator, rex(space))
      is_comma <- re_matches(two_before_operator, rex(","))

      if (non_space_before && !is_comma || non_space_after) {
        Lint(
          filename = source_file$filename,
          line_number = parsed$line1,
          column_number = parsed$col1,
          type = "style",
          message = "Do not place spaces around code in parentheses or square brackets.",
          line = getSrcLines(source_file, parsed$line1, parsed$line1)
          )
      }
    })
}
