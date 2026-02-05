#' Trailing whitespace linter
#'
#' Check that there are no space characters at the end of source lines.
#'
#' @param allow_empty_lines Suppress lints for lines that contain only whitespace.
#' @param allow_in_strings Suppress lints for trailing whitespace in string constants.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x <- 1.2  ",
#'   linters = trailing_whitespace_linter()
#' )
#'
#' code_lines <- "a <- TRUE\n \nb <- FALSE"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = trailing_whitespace_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x <- 1.2",
#'   linters = trailing_whitespace_linter()
#' )
#'
#' lint(
#'   text = "x <- 1.2  # comment about this assignment",
#'   linters = trailing_whitespace_linter()
#' )
#'
#' code_lines <- "a <- TRUE\n \nb <- FALSE"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = trailing_whitespace_linter(allow_empty_lines = TRUE)
#' )
#'
#' @evalRd rd_tags("trailing_whitespace_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
trailing_whitespace_linter <- function(allow_empty_lines = FALSE, allow_in_strings = TRUE) {
  Linter(linter_level = "file", function(source_expression) {
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

    if (isTRUE(allow_in_strings) && !is.null(source_expression$full_xml_parsed_content)) {
      all_str_consts <- xml_find_all(source_expression$full_xml_parsed_content, "//STR_CONST")
      start_lines <- as.integer(xml_attr(all_str_consts, "line1"))
      end_lines <- as.integer(xml_attr(all_str_consts, "line2"))

      is_in_str <- vapply(bad_lines, \(ln) any(start_lines <= ln & ln < end_lines), logical(1L))
      bad_lines <- bad_lines[!is_in_str]
    }

    lapply(
      bad_lines,
      function(line) {
        Lint(
          filename = source_expression$filename,
          line_number = line,
          column_number = res$start[[line]],
          type = "style",
          message = "Remove trailing whitespace.",
          line = source_expression$file_lines[[line]],
          ranges = list(c(res$start[[line]], res$end[[line]]))
        )
      }
    )
  })
}
