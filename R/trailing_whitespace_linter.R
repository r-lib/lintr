#' Trailing whitespace linter
#'
#' Check that there are no space characters at the end of source lines.
#'
#' @param allow_empty_lines Suppress lints for lines that contain only whitespace.
#'
#' @evalRd rd_tags("trailing_whitespace_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
trailing_whitespace_linter <- function(allow_empty_lines = FALSE) {
  Linter(function(source_expression) {
    if (is.null(source_expression$file_lines)) return(list())

    res <- re_matches(
      source_expression$file_lines,
      rex(blanks, end),
      locations = TRUE
    )

    if (isTRUE(allow_empty_lines)) {
      bad_lines <- which(res$start > 1L)
    } else {
      bad_lines <- which(!is.na(res$start))
    }

    lapply(
      bad_lines,
      function(line) {
        Lint(
          filename = source_expression$filename,
          line_number = line,
          column_number = res$start[[line]],
          type = "style",
          message = "Trailing whitespace is superfluous.",
          line = source_expression$file_lines[[line]],
          ranges = list(c(res$start[[line]], res$end[[line]]))
        )
      }
    )
  })
}
