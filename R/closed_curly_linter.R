#' @describeIn linters check that closed curly braces should always be on their
#' own line unless they follow an else.
#' @param allow_single_line if true allow a open and closed curly pair on the
#' same line.
#' @export
closed_curly_linter <- function(allow_single_line = FALSE) {
  function(source_file) {
    lapply(ids_with_token(source_file, "'}'"),
           function(id) {

             parsed <- with_id(source_file, id)

             tokens_before <- source_file$parsed_content$token[
                                                               source_file$parsed_content$line1 == parsed$line1 &
                                                               source_file$parsed_content$col1 < parsed$col1]

             tokens_after <- source_file$parsed_content$token[
                                                              source_file$parsed_content$line1 == parsed$line1 &
                                                              source_file$parsed_content$col1 > parsed$col1]
             if (isTRUE(allow_single_line) &&
                 "'{'" %in% tokens_before) {
               return()
             }

             has_expression_before <- any(tokens_before %in% "expr")

             has_expression_after <- any(tokens_after %in% "expr")

             has_else_after <- any(tokens_after %in% "ELSE")

             # if the closing curly has an expression on the same line, and there is
             # not also an else
             if (has_expression_before ||
                 has_expression_after && !has_else_after) {
               Lint(
                    filename = source_file$filename,
                    line_number = parsed$line1,
                    column_number = parsed$col1,
                    type = "style",
                    message = "Closing curly-braces should always be on their own line, unless it's followed by an else.", # nolint
                    line = source_file$lines[as.character(parsed$line1)],
                    linter = "closed_curly_linter"
                    )
             }

           })
  }
}
