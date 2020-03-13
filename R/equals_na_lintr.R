#' @describeIn linters that checks for x == NA
#' @include   make_linter_from_regex.R
#'
#' @export
equals_na_linter <- make_linter_from_regex(
  regex = rex("==", zero_or_more(" "), "NA"),
  lint_name = "equals_na_linter",
  lint_type = "warning",
  lint_msg = "Use is.na rather than == NA."
)
