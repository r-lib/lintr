#' Trailing assignment linter
#'
#' Check that there are no assignment operators at the end of source lines.
#'
#' @param allow_piping Suppress lints when assignment begins/ends a pipe
#' @param allow_comments Suppress lints for commented out lines
#'
#' @evalRd rd_tags("trailing_whitespace_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
trailing_assignment_linter <- function(allow_piping = TRUE, allow_comments = FALSE) {
  left_assign_pipe_xml <- paste(
    # In xml derived from the expression `E1 <-\n  E2 %>%\n  E3`:
    # `E1`, <-`, `E2`, ``%>%` and `E3` are sibling expressions of the parent expression

    # select all left assignments
    "//LEFT_ASSIGN[",
    # that are nested in a parent-expression that spans multiple lines
    "parent::expr[@line1 < @line2]",
    # where the parent contains pipes that follow the pipe under scrutiny
    "and following-sibling::*/descendant-or-self::SPECIAL[text() = '%>%']",
    "]"
  )

  right_assign_pipe_xml <- paste(
    # In xml derived from the expression `E1 %>%\n  E2 ->\n  E3`:
    # `E1`, `%>%`, `E2`, `->` and `E3` are sibling expressions of the parent expression

    # select all right assignments
    "//RIGHT_ASSIGN[",
    # that are nested in a parent-expression that spans multiple lines
    "parent::expr[@line1 < @line2]",
    # where the parent contains pipes that precede the pipe under scrutiny
    "and preceding-sibling::*/descendant-or-self::SPECIAL[text() = '%>%']",
    "]"
  )

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    res <- re_matches(
      source_expression$file_lines,
      rex(capture(or("<<-", "<-", "=", "->", "->>")), any_spaces, end)
    )
    bad_lines <- which(!vapply(res[, 1L], is.na, logical(1L), USE.NAMES = FALSE))

    if (allow_piping && length(bad_lines) > 0L) {
      xml <- source_expression$full_xml_parsed_content

      l_assign_result <- xml2::xml_find_all(xml, left_assign_pipe_xml)
      l_assign_lines <- as.integer(xml2::xml_attr(l_assign_result, "line1"))

      r_assign_result <- xml2::xml_find_all(xml, right_assign_pipe_xml)
      r_assign_lines <- as.integer(xml2::xml_attr(r_assign_result, "line1"))

      bad_lines <- setdiff(bad_lines, c(l_assign_lines, r_assign_lines))
    }

    if (allow_comments && length(bad_lines) > 0L) {
      comment_lines <- grep("#", source_expression$file_lines, fixed = TRUE)
      bad_lines <- setdiff(bad_lines, comment_lines)
    }

    lapply(
      seq_along(bad_lines),
      function(x) {
        line <- bad_lines[x]
        Lint(
          filename = source_expression$filename,
          line_number = line,
          column_number = 1L,
          type = "style",
          message = paste0(
            "Assignment `", res[line, 1L], "` should not be trailing at end of line"
          ),
          line = source_expression$file_lines[[line]]
        )
      }
    )
  })
}
