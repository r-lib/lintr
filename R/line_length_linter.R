#' Line length linter
#'
#' Check that the line length of both comments and code is less than `length`.
#'
#' @param length maximum line length allowed.
#' @evalRd rd_tags("line_length_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#long-lines>
#' @export
line_length_linter <- function(length = 80L) {
  Linter(function(source_expression) {

    # Only go over complete file
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    line_lengths <- nchar(source_expression$file_lines)
    long_lines <- which(line_lengths > length)

    lint_message <- sprintf("Lines should not be more than %d characters.", length)

    lapply(long_lines, function(long_line) {
      Lint(
        filename = source_expression$filename,
        line_number = long_line,
        column_number = length + 1L,
        type = "style",
        message = lint_message,
        line = source_expression$file_lines[long_line],
        ranges = list(c(1L, line_lengths[long_line]))
      )
    })
  })
}
