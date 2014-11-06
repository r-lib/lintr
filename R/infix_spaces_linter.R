infix_operators <- c(

  "'+'",
  "'-'",
  "GT",
  "GE",
  "LT",
  "LE",
  "EQ",
  "NE",
  "AND",
  "OR",
  "AND2",
  "OR2",
  "LEFT_ASSIGN",
  "RIGHT_ASSIGN",
  "EQ_ASSIGN",
  "SPECIAL",
  "'/'",
  "'^'",
  "'*'",

  NULL
  )

#' @describeIn linters check that all infix operators have spaces around them.
#' @export
infix_spaces_linter <- function(source_file) {
  lapply(ids_with_token(source_file, infix_operators, fun=`%in%`),
    function(id) {
      parsed <- source_file$parsed_content[id, ]
      line <- source_file$lines[parsed$line1]
      before_operator <- substr(line, parsed$col1 - 1L, parsed$col1 - 1L)
      after_operator <-
        substr(line,
          parsed$col1 + nchar(parsed$text),
          parsed$col1 + nchar(parsed$text))

      non_space_before <- re_matches(before_operator, rex(non_space))
      non_space_after <- re_matches(after_operator, rex(non_space))

      # we only should check spacing if the operator is infix,
      # which only happens if there are two siblings
      is_infix <-
        length(siblings(source_file$parsed_content, parsed$id, 1)) %==% 2L

      if (is_infix &&
        (non_space_before || non_space_after)
        ) {
        start <- parsed$col1
        if (non_space_before) {
          start <- parsed$col1 - 1L
        }

        end <- parsed$col1
        if (non_space_after) {
          end <- parsed$col1 + nchar(parsed$text)
        }

        Lint(
          filename = source_file$filename,
          line_number = parsed$line1,
          column_number = parsed$col1,
          type = "style",
          message = "Put spaces around all infix operators.",
          line = line,
          ranges = list(c(start, end))
          )

      }

    })
}
