#' @describeIn linters check that all commas are followed by spaces, but do not
#' have spaces before them.
commas_linter <- function(source_file) {
  lapply(which(source_file$parsed_content$token == "','"),
    function(id) {
      parsed <- source_file$parsed_content[id, ]
      line <- getSrcLines(source_file, parsed$line1, parsed$line1)
      before_comma <- substr(line, 1L, parsed$col1 - 1L)
      after_comma <- substr(line, parsed$col1 + 1L, parsed$col1 + 1L)

      res <- list()

      before_match <-
        re_matches(before_comma,
          rex(non_space,
            spaces,
            end),
          locations = TRUE)

      if (!is.na(before_match$start)) {

        res[[length(res) + 1L]] <-
          Lint(
            filename = source_file$filename,
            line_number = parsed$line1,
            column_number = before_match$start + 1L,
            type = "style",
            message = "Commas should never have a space before.",
            line = getSrcLines(source_file, parsed$line1, parsed$line1),
            ranges = list(c(before_match$start + 1L, before_match$end))
            )

      }

      if (re_matches(after_comma, rex(non_spaces))) {

        res[[length(res) + 1L]] <-
          Lint(
            filename = source_file$filename,
            line_number = parsed$line1,
            column_number = parsed$col1 + 1L,
            type = "style",
            message = "Commas should always have a space after.",
            line = getSrcLines(source_file, parsed$line1, parsed$line1)
            )

      }

      res
    })
}
