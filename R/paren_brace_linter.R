#' @describeIn linters check that there is a space between right
#' parenthesis and an opening curly brace.
#' @include   make_linter_from_regex.R
#'
#' @export

paren_brace_linter <- make_linter_from_regex(
  regex = rex("){"),
  lint_name = "paren_brace_linter",
  lint_type = "style",
  lint_msg = "There should be a space between right parenthesis and an opening curly brace."
)
