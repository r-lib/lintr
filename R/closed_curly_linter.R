#' @describeIn linters  Check that closed curly braces are on their own line
#'   unless they follow an else.
#' @param allow_single_line if \code{TRUE}, allow an open and closed curly pair
#'   on the same line.
#' @export
closed_curly_linter <- function(allow_single_line = FALSE) {
  Linter(function(source_file) {
    lapply(ids_with_token(source_file, "'}'"),
           function(id) {

        parsed <- with_id(source_file, id)
        parsed_content <- source_file[["parsed_content"]]

        tokens_before <- parsed_content$token[
                                              parsed_content$line1 == parsed$line1 &
                                              parsed_content$col1 < parsed$col1]

        tokens_after <- parsed_content$token[
                                             parsed_content$line1 == parsed$line1 &
                                             parsed_content$col1 > parsed$col1]
        if (isTRUE(allow_single_line) &&
            "'{'" %in% tokens_before) {
          return()
        }

        if (length(tokens_after) &&
            tokens_after[[1]] %in% c("')'", "','")) {
          return()
        }

        has_expression_before <- any(tokens_before %in% "expr")

        has_expression_after <- any(tokens_after %in% "expr")

        has_else_after <- any(tokens_after %in% "ELSE")

        line <- source_file$lines[as.character(parsed$line1)]
        content_after <- unname(substr(line, parsed$col1 + 1L, nchar(line)))
        content_before <- unname(substr(line, 1, parsed$col1 - 1L))

        double_curly <- rex::re_matches(content_after, rex::rex(start, "}")) ||
          rex::re_matches(content_before, rex::rex("}", end))

        if (double_curly) {
          return()
        }

        # If the closing curly has an expression on the same line, and there is
        # not also an else
        if (has_expression_before ||
            has_expression_after && !has_else_after) {
          Lint(
            filename = source_file$filename,
            line_number = parsed$line1,
            column_number = parsed$col1,
            type = "style",
            message = paste(
              "Closing curly-braces should always be on their own line,",
              "unless they are followed by an else."
            ),
            line = source_file$lines[as.character(parsed$line1)]
          )}
      }
    )
  })
}
