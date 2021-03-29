#' @describeIn linters Check that only spaces are used for indentation, not tabs.
#' @include make_linter_from_regex.R
#'
#' @export
no_tab_linter <- make_linter_from_regex(
  regex = rex(start, zero_or_more(regex("\\s")), one_or_more("\t")),
  lint_type = "style",
  lint_msg = "Use spaces to indent, not tabs."
)
