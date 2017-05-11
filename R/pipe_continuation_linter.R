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
          message = "`%>%` should always have a space before it and a new line after it, unless the full pipeline fits on one line.",
          line = line,
          ranges = list(as.numeric(c(x@col1, x@col2))),
          "pipe_continuation_linter"
          )
      })
  })
}

utils::globalVariables(c("line1", "col1", "col2"), "lintr")
