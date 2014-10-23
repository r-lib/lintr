spaces_left_parentheses_linter <- function(source_file) {
  lapply(which(source_file$parsed_content$token %in% "'('"),
    function(id) {

      parsed <- source_file$parsed_content[id, ]

      family_ids <- family(source_file$parsed_content, parsed$id)

      types <- source_file$parsed_content[
          source_file$parsed_content$id %in% family_ids,
          "token"]

      is_function <- length(family_ids) %!=% 0L &&
        any(types %in% c("SYMBOL_FUNCTION_CALL", "FUNCTION"))

      if (!is_function) {

        line <- getSrcLines(source_file, parsed$line1, parsed$line1)

        before_operator <- substr(line, parsed$col1 - 1L, parsed$col1 - 1L)

        non_space_before <- re_matches(before_operator, rex(non_space))

        if (non_space_before) {
          Lint(
            filename = source_file$filename,
            line_number = parsed$line1,
            column_number = parsed$col1,
            type = "style",
            message = "Place a space before left parenthesis, except in a function call.",
            line = getSrcLines(source_file, parsed$line1, parsed$line1)
            )
        }
      }

    })
}
