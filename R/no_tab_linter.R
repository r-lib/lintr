#' No tab linter
#'
#' Check that only spaces are used for indentation, not tabs. Much ink has been
#' spilt on this topic, and we encourage you to check out references for more
#' information.
#'
#' @include make_linter_from_regex.R
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "\tx",
#'   linters = no_tab_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "  x",
#'   linters = no_tab_linter()
#' )
#'
#' @evalRd rd_tags("no_tab_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#'
#' @references
#' - https://softwareengineering.stackexchange.com/questions/57/tabs-versus-spaces-what-is-the-proper-indentation-character-for-everything-in-e
#' - https://www.jwz.org/doc/tabs-vs-spaces.html
#' - https://blog.codinghorror.com/death-to-the-space-infidels/
#' @export
no_tab_linter <- make_linter_from_regex(
  regex = rex(start, zero_or_more(regex("\\s")), one_or_more("\t")),
  lint_type = "style",
  lint_msg = "Use spaces to indent, not tabs."
)
