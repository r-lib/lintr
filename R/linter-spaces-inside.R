#' @describeIn linters check that parentheses and square brackets do not have
#' spaces directly inside them.
#' @export
spaces_inside_linter <- function(source_file) {
  matches <- c(
    "'('",
    "')'",
    "'['",
    "']'")

  lapply(which(source_file$parsed_content$token %in% matches),
    function(id) {

      parsed <- source_file$parsed_content[id, ]

      is_open <- parsed$token %in% matches[c(1, 3)]

      line <- getSrcLines(source_file, parsed$line1, parsed$line1)

      if (is_open) {
        after_operator <- substr(line, parsed$col1 + 1L, parsed$col1 + 1L)

        has_space <- re_matches(after_operator, rex(space))
      }
      else {
        before_operator <- substr(line, 1L, parsed$col1 - 1L)

        has_space <- re_matches(before_operator,
          rex(non_space,

            # need to special case ', '
            spaces %if_prev_isnt% c(","),

            end))
      }


      if (has_space) {
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
