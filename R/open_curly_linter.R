#' @describeIn linters check that opening curly braces are never on their own
#' line and are always followed by a newline.
#' @export
open_curly_linter <- function(allow_single_line = FALSE) {
  function(source_file) {
    lapply(ids_with_token(source_file, "'{'"),
           function(id) {

             parsed <- with_id(source_file, id)

             tokens_before <- source_file$parsed_content$token[
                                                               source_file$parsed_content$line1 == parsed$line1 &
                                                               source_file$parsed_content$col1 < parsed$col1]

             tokens_after <- source_file$parsed_content$token[
                                                              source_file$parsed_content$line1 == parsed$line1 &
                                                              source_file$parsed_content$col1 > parsed$col1]

             if (isTRUE(allow_single_line) &&
                 "'}'" %in% tokens_after) {
               return()
             }

             line <- source_file$lines[as.character(parsed$line1)]

             # the only tokens should be the { and the start of the expression.
             some_before <- length(tokens_before) %!=% 0L
             some_after <- length(tokens_after) %!=% 0L

             whitespace_after <-
               unname(substr(line, parsed$col1 + 1L, parsed$col1 + 1L)) %!=% ""

             if (!some_before || some_after || whitespace_after) {
               Lint(
                    filename = source_file$filename,
                    line_number = parsed$line1,
                    column_number = parsed$col1,
                    type = "style",
                    message = "Opening curly braces should never go on their own line and should always be followed by a new line.", # nolint
                    line = line,
                    linter = "open_curly_linter"
                    )
             }

           })
  }
}
