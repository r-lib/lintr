#' Function left parentheses linter
#'
#' Check that all left parentheses in a function call do not have spaces before them.
#'
#' @evalRd rd_tags("function_left_parentheses_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#parentheses>
#' @export
function_left_parentheses_linter <- function() { # nolint: object_length.
  Linter(function(source_expression) {
    lapply(
      ids_with_token(source_expression, "'('"),
      function(id) {

        parsed <- source_expression$parsed_content[id, ]

        terminal_tokens_before <-
          source_expression$parsed_content$line1 == parsed$line1 &
            source_expression$parsed_content$col1 < parsed$col1 &
            source_expression$parsed_content$terminal

        last_type <- tail(source_expression$parsed_content$token[terminal_tokens_before], n = 1L)

        is_function_call <- length(last_type) %!=% 0L &&
          (last_type %in% c("SYMBOL_FUNCTION_CALL", "FUNCTION", "'}'", "']'"))

        if (is_function_call) {

          line <- source_expression$lines[as.character(parsed$line1)]

          before_operator <- substr(line, parsed$col1 - 1L, parsed$col1 - 1L)

          space_before <- re_matches(before_operator, rex(space))

          if (space_before) {
            Lint(
              filename = source_expression$filename,
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
