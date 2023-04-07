#' Whitespace linter
#'
#' Check that the correct character is used for indentation.
#'
#' Currently, only supports linting in the presence of tabs.
#'
#' Much ink has been spilled on this topic, and we encourage you to check
#'   out references for more information.
#'
#' @include make_linter_from_regex.R
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "\tx",
#'   linters = whitespace_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "  x",
#'   linters = whitespace_linter()
#' )
#'
#' @evalRd rd_tags("whitespace_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#'
#' @references
#' - https://www.jwz.org/doc/tabs-vs-spaces.html
#' - https://blog.codinghorror.com/death-to-the-space-infidels/
#' @export
whitespace_linter <- make_linter_from_regex(
  regex = rex(start, zero_or_more(regex("\\s")), one_or_more("\t")),
  lint_type = "style",
  lint_msg = "Use spaces to indent, not tabs."
)
