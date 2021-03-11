#' @describeIn linters check that all left parentheses in a function call
#' do not have spaces before them.
#' @export
function_left_parentheses_linter <- function() { # nolint: object_length_linter.
  Linter(function(source_file) {
    lapply(
      ids_with_token(source_file, "'('"),
      function(id) {

        parsed <- source_file$parsed_content[id, ]

        terminal_tokens_before <-
          source_file$parsed_content$line1 == parsed$line1 &
            source_file$parsed_content$col1 < parsed$col1 &
            source_file$parsed_content$terminal

        last_type <- tail(source_file$parsed_content$token[terminal_tokens_before], n = 1)

        is_function_call <- length(last_type) %!=% 0L &&
          (last_type %in% c("SYMBOL_FUNCTION_CALL", "FUNCTION", "'}'", "']'"))

        if (is_function_call) {

          line <- source_file$lines[as.character(parsed$line1)]

          before_operator <- substr(line, parsed$col1 - 1L, parsed$col1 - 1L)

          space_before <- re_matches(before_operator, rex(space))

          if (space_before) {
            Lint(
              filename = source_file$filename,
              line_number = parsed$line1,
              column_number = parsed$col1,
              type = "style",
              message = "Remove spaces before the left parenthesis in a function call.",
              line = line
            )
          }
        }
      }
    )
  })
}
