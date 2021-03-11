#' @describeIn linters Check that only single quotes are used to delimit
#' string constants.
#' @export
single_quotes_linter <- function() {
  Linter(function(source_file) {
    if (is.null(source_file$full_parsed_content)) {
      return(list())
    }

    content <- source_file$full_parsed_content
    str_idx <- which(content$token == "STR_CONST")
    squote_matches <- which(re_matches(
      content[str_idx, "text"],
      rex(start, single_quote, any_non_double_quotes, single_quote, end)
    ))

    lapply(
      squote_matches,
      function(id) {
        with(content[str_idx[id], ], {
          line <- source_file$file_lines[line1]
          col2 <- if (line1 == line2) col2 else nchar(line)
          Lint(
            filename = source_file$filename,
            line_number = line1,
            column_number = col1,
            type = "style",
            message = "Only use double-quotes.",
            line = line,
            ranges = list(c(col1, col2))
          )
        })
      }
    )
  })
}
