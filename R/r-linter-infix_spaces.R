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

infix_spaces_linter <- function(source_file) {
  lapply(which(source_file$parsed_content$token %in% infix_operators),
    function(id) {
      parsed <- source_file$parsed_content[id, ]
      line <- getSrcLines(source_file, parsed$line1, parsed$line1)
      before_operator <- substr(line, parsed$col1 - 1L, parsed$col1 - 1L)
      after_operator <-
        substr(line,
          parsed$col1 + nchar(parsed$text),
          parsed$col1 + nchar(parsed$text))

      non_space_before <- re_matches(before_operator, rex(non_space))
      non_space_after <- re_matches(after_operator, rex(non_space))

      res <- list()

      # Need to special case - and + because they can be used before a number
      if (
        (parsed$token %in% infix_operators[1:2] &&
          non_space_before) ||
        (!parsed$token %in% infix_operators[1:2] &&
          (non_space_before || non_space_after))
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
          line = getSrcLines(source_file, parsed$line1, parsed$line1),
          ranges = list(c(start, end))
          )

      }

    })
}
