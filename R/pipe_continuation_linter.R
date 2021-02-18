#' @describeIn linters Check that each step in a pipeline is on a new line, or
#' the entire pipe fits on one line.
#' @importFrom xml2 xml_find_all as_list
#' @export
pipe_continuation_linter <- function() {
  Linter(function(source_file) {
    x <- global_xml_parsed_content(source_file)
    if (is.null(x)) {
      return()
    }

    # For pipelines, either the whole expression should fit on a single line, or,
    # each pipe symbol within the expression should be on a separate line

    # Where a single-line pipeline is nested inside a larger expression (eg,
    # inside a function definition), the outer expression can span multiple lines
    # without a pipe-continuation lint being thrown.

    multiline_pipe_test <- paste(
      # In xml derived from the expression `E1 %>% E2`:
      # `E1`, the pipe and `E2` are sibling expressions of the parent expression

      # We only consider expressions where E1 contains a pipe character somewhere
      # and we split these expressions based on whether E2 starts on the same
      # line, or on a subsequent line to that where E1 ended.

      # We only consider the parent expression of the pipe under consideration,
      # rather than all ancestors.

      # select all pipes
      "SPECIAL[text() = '%>%'",
      # that are nested in a parent-expression that spans multiple lines
      "and parent::expr[@line1 < @line2]",
      # where the parent contains pipes that precede the pipe under scrutiny
      "and preceding-sibling::*/descendant-or-self::SPECIAL[text() = '%>%']",
      "and (",
      # where a preceding sibling 'expr' [...] ends on the same line that a
      # succeeding 'expr' starts {...}
      # either "[a %>%\n b()] %>% {c(...)}"
      # or     "[a %>% b(...\n)] %>% {c(...)}"
      "(preceding-sibling::*/descendant-or-self::expr/@line2 = ",
      "  following-sibling::*/descendant-or-self::expr/@line1)",

      # or, (if the post-pipe expression starts on a subsequent line) where a
      # preceding sibling 'expr' [...] contains a pipe character on the
      # same line as the original pipe character |...|
      # [a %>% b] |%>%| \n c
      "or (@line1 = preceding-sibling::*/descendant-or-self::SPECIAL[text() = '%>%']/@line1)",
      ")",
      "]"
    )

    pipe_exprs <- xml_find_all(x, p("//", multiline_pipe_test))

    lapply(
      pipe_exprs,
      function(expr) {
        x <- as_list(expr)
        line <- get_file_line(source_file, x@line1)
        Lint(
          filename = source_file$filename,
          line_number = x@line1,
          column_number = x@col2,
          type = "style",
          message = p(
            "`%>%` should always have a space before it and a new line after it,",
            " unless the full pipeline fits on one line."
          ),
          line = line,
          ranges = list(as.numeric(c(x@col1, x@col2)))
        )
      })
  })
}
