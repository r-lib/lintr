#' @describeIn linters checks that only single quotes are used to delimit
#' string contestants.
#' @export
single_quotes_linter <- function(source_file) {
  lapply(ids_with_token(source_file, "STR_CONST"),
    function(id) {
      parsed <- with_id(source_file, id)
      if (re_matches(parsed$text, rex(start, single_quote, any_non_double_quotes, single_quote, end))) {
        Lint(
          filename = source_file$filename,
          line_number = parsed$line1,
          column_number = parsed$col1,
          type = "style",
          message = "Only use double-quotes.",
          line = source_file$lines[as.character(parsed$line1)],
          ranges = list(c(parsed$col1, parsed$col2)),
          linter = "single_quotes_linter"
          )
      }
    })
}
