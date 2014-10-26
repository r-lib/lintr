#' Available linters
#'
#' @name linters
#' @title linters
#' @param source_file returned by \code{\link{get_source_file}}
#' @param width the width cutoff to use for the given linter.
NULL

default_linters <- c(

  assignment_linter,
  single_quotes_linter,
  absolute_paths_linter,
  no_tab_linter,
  line_length_linter(80),
  commas_linter,
  infix_spaces_linter,
  spaces_left_parentheses_linter,
  spaces_inside_linter,
  open_curly_linter,
  closed_curly_linter,
  object_name_linter(30),

  NULL
)
