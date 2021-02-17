#' @describeIn linters  Check that opening curly braces are never on their own
#' line and are always followed by a newline.
#' @export
open_curly_linter <- function(allow_single_line = FALSE) {
  Linter(function(source_file) {
    lapply(
      ids_with_token(source_file, "'{'"),
      function(id) {

        parsed <- with_id(source_file, id)

        tokens_before <- source_file$parsed_content$token[
          source_file$parsed_content$line1 == parsed$line1 &
            source_file$parsed_content$col1 < parsed$col1]

        tokens_after <- source_file$parsed_content$token[
          source_file$parsed_content$line1 == parsed$line1 &
            source_file$parsed_content$col1 > parsed$col1 &
            source_file$parsed_content$token != "COMMENT"]

        if (isTRUE(allow_single_line) &&
            "'}'" %in% tokens_after) {
          return()
        }

        line <- source_file$lines[as.character(parsed$line1)]

        # the only tokens should be the { and the start of the expression.
        some_before <- length(tokens_before) %!=% 0L
        some_after <- length(tokens_after) %!=% 0L

        content_after <- unname(substr(line, parsed$col1 + 1L, nchar(line)))
        content_before <- unname(substr(line, 1, parsed$col1 - 1L))

        only_comment <- rex::re_matches(content_after, rex::rex(any_spaces, "#", something, end))

        double_curly <- rex::re_matches(content_after, rex::rex(start, "{")) ||
          rex::re_matches(content_before, rex::rex("{", end))

        if (double_curly) {
          return()
        }

        whitespace_after <-
          unname(substr(line, parsed$col1 + 1L, parsed$col1 + 1L)) %!=% ""

        if (!some_before ||
          some_after ||
          (whitespace_after && !only_comment)) {
          Lint(
            filename = source_file$filename,
            line_number = parsed$line1,
            column_number = parsed$col1,
            type = "style",
            message = paste(
              "Opening curly braces should never go on their own line and",
              "should always be followed by a new line."
            ),
            line = line
          )
        }
      }
    )
  })
}
