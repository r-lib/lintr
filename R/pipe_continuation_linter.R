#' @describeIn linters check that all commas are followed by spaces, but do not
#' have spaces before them.
#' @importFrom xml2 xml_find_all as_list
#' @export
pipe_continuation_linter <- function(source_file) {
  x <- global_xml_parsed_content(source_file)
  if (is.null(x)) {
    return()
  }

  # Find all expressions that span more than one line
  # and contain specials named '%>%' that are on the same line

  expressions_spanning_multiple_lines <- "expr[@line1 != @line2]"
  pipes_on_same_line <- "SPECIAL[text() = '%>%' and @line1 = following::SPECIAL[text() = '%>%']/@line1]"

  exprs <- xml_find_all(x,
    p("//",
      expressions_spanning_multiple_lines,
      "//",
      pipes_on_same_line
    ))

  lapply(exprs,
    function(expr) {
      x <- as_list(expr)
      line <- get_file_line(source_file, x@line1)
      Lint(
        filename = source_file$filename,
        line_number = x@line1,
        column_number = x@col2,
        type = "style",
        message = "Each `%>%` statement should be on it's own line.",
        line = line,
        ranges = list(as.numeric(c(x@col1, x@col2))),
        "pipe_continuation_linter"
        )
    })
}

utils::globalVariables(c("line1", "col1", "col2"), "lintr")
