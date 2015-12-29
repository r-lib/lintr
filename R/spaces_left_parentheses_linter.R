#' @describeIn linters check that all left parentheses have a space before them
#' unless they are in a function call.
#' @export
spaces_left_parentheses_linter <- function(source_file) {
  lapply(ids_with_token(source_file, "'('"),
    function(id) {

      parsed <- source_file$parsed_content[id, ]

      terminal_tokens_before <-
        source_file$parsed_content$token[
                                         source_file$parsed_content$line1 == parsed$line1 &
                                         source_file$parsed_content$col1 < parsed$col1 &
                                         source_file$parsed_content$terminal]
      last_type <- tail(terminal_tokens_before, n = 1)

      is_function <- length(last_type) %!=% 0L &&
        (last_type %in% c("SYMBOL_FUNCTION_CALL", "FUNCTION", "'}'", "')'", "']'"))

      if (!is_function) {

        line <- source_file$lines[as.character(parsed$line1)]

        before_operator <- substr(line, parsed$col1 - 1L, parsed$col1 - 1L)

        non_space_before <- re_matches(before_operator, rex(non_space))
        not_exception <- !(before_operator %in% c("!", ":", "["))

        if (non_space_before && not_exception) {
          Lint(
            filename = source_file$filename,
            line_number = parsed$line1,
            column_number = parsed$col1,
            type = "style",
            message = "Place a space before left parenthesis, except in a function call.",
            line = line,
            linter = "spaces_left_parentheses_linter"
            )
        }
      }

    })
}
