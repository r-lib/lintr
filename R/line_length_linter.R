#' Line length linter
#'
#' Check that the line length of both comments and code is less than `length`.
#'
#' @param length maximum line length allowed. Default is 80L (Hollerith limit).
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = strrep("x", 23L),
#'   linters = line_length_linter(length = 20L)
#' )
#'
#' # okay
#' lint(
#'   text = strrep("x", 21L),
#'   linters = line_length_linter(length = 40L)
#' )
#'
#' @evalRd rd_tags("line_length_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#long-lines>
#' @export
line_length_linter <- function(length = 80L) {
  general_msg <- paste("Lines should not be more than", length, "characters.")

  Linter(linter_level = "file", function(source_expression) {
    # Only go over complete file
    line_lengths <- nchar(source_expression$file_lines)
    long_lines <- which(line_lengths > length)

    Map(
      function(long_line, line_length) {
        Lint(
          filename = source_expression$filename,
          line_number = long_line,
          column_number = length + 1L,
          type = "style",
          message = paste(general_msg, "This line is", line_length, "characters."),
          line = source_expression$file_lines[long_line],
          ranges = list(c(1L, line_length))
        )
      },
      long_lines,
      line_lengths[long_lines]
    )
  })
}
