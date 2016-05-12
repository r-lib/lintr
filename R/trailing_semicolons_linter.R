#' @describeIn linters check there are no trailing semicolons.
#' @export
trailing_semicolons_linter <- function(source_file) {

  allsc <- which(source_file$parsed_content$token == "';'")

  if (!length(allsc)) return(list())

  ## Check that it is the last token in the line
  ## Note that we need to restrict the search to terminal
  ## nodes, because parse is buggy and sometimes reports false,
  ## too large end positions for non-terminal nodes.
  badsc <- Filter(
    x = allsc,
    f = function(line) {
      with(
        source_file$parsed_content,
        all(! terminal | line1 != line1[line] | col2 <= col2[line])
      )
    }
  )

  lapply(
    badsc,
    function(line) {
      parsed <- source_file$parsed_content[line, ]
      Lint(
        filename = source_file$filename,
        line_number = parsed$line1,
        column_number = parsed$col1,
        type = "style",
        message = "Avoid trailing semicolons, they are not needed.",
        line = source_file$lines[as.character(parsed$line1)],
        ranges = list(c(parsed$col1, parsed$col2)),
        linter = "trailing_semicolons_linter"
      )
    }
  )
}
